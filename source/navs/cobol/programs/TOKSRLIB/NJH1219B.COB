****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　　*
*    サブシステム　　　　：　コーナン　ＥＤＩ　　　　　　　　　*
*    モジュール名　　　　：　コーナン発注データ変換　　　　　　*
*    　　　　　　　　　　　　（ＰＯＲケース）　　　　　　　　　*
*    作成日／作成者　　　：　2021/01/08 INOUE                  *
*    処理概要　　　　　　：　コーナンＰＯＲ／ケースデータから　*
*                            ケースＰＤラベルファイルを作成する*
*                            コーナン商事用                    *
*    変更日／変更者　　　：　2021/05/18 INOUE                  *
*    変更概要　　　　　　：　KNCASEL4　KEY追加
*    変更日／変更者　　　：　2023/08/28 NA TAKAHASHI           *
*    変更概要　　　　　　：　得意先ＣＤ枝番対応
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH1219B.
*                  流用:NJH1208B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/08.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＰＯＲ／ケースデータ
     SELECT   KONANPOR  ASSIGN    TO        DA-01-S-KONANPOR
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*コーナン基本情報ファイル
     SELECT   KNJOHOF   ASSIGN    TO        DA-01-VI-KNJOHOL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F001  JOH-F002
                                            JOH-FA07  JOH-FA08
                                            JOH-FA03  JOH-FA04
                                            JOH-FA05  JOH-FA06
                                            JOH-FB11
                        FILE  STATUS   IS   JOH-STATUS.
*コーナン　ケースＰＤラベルファイル
     SELECT   KNCASEF   ASSIGN    TO        DA-01-VI-KNCASEL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       CAS-F001  CAS-F002
                                            CAS-FA07  CAS-FA08
                                            CAS-FA03  CAS-FA04
                                            CAS-FA05  CAS-FA06
                                            CAS-FB11  CAS-FD05
*2021/05/18↓
                                            CAS-FD03
*2021/05/18↑
                        FILE  STATUS   IS   CAS-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　ＢＦ＝　１
******************************************************************
 FD  KONANPOR
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
*    03  DEN-01                  OCCURS 2.
     03  DEN-01.
         05  DEN-01A             PIC  X(02).
         05  DEN-01B             PIC  X(126).
******************************************************************
*    コーナン基本情報ファイル
******************************************************************
 FD  KNJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     KNJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    コーナン　ケースＰＤラベルファイル
******************************************************************
 FD  KNCASEF            LABEL RECORD   IS   STANDARD.
     COPY     KNCASEF   OF        XFDLIB
              JOINING   CAS       PREFIX.
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  RWT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WRT-JYR-CNT             PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TEN-F73              PIC  X(02)     VALUE  SPACE.
 01  KNJOHOL4-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  KNCASEL4-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
*
*    ケースＰＤラベルレコード退避ワーク
 01  WK-PPDT-REC.
     03  WK-PPDT01                PIC       X(02).
     03  WK-PPDT02                PIC       9(08).
     03  WK-PPDT03                PIC       X(20).
     03  WK-PPDT04                PIC       X(13).
     03  WK-PPDT05                PIC       9(04).
     03  WK-PPDT06                PIC       9(02).
     03  WK-PPDT07                PIC       9(06).
     03  WK-PPDT08                PIC       9(06).
     03  WK-PPDT09                PIC       9(06).
     03  WK-PPDT10                PIC       9(02).
     03  WK-PPDT11                PIC       X(20).
     03  WK-PPDT12                PIC       X(12).
     03  WK-PPDT13                PIC       X(03).
     03  WK-PPDT14                PIC       9(01).
     03  WK-PPDT15                PIC       9(08).
     03  WK-PPDT16                PIC       9(08).
     03  WK-PPDT17                PIC       9(01).
     03  WK-PPDT18                PIC       9(06).
*
 01  WK-AREA.
     03  WK-GYO            PIC 9(02).
     03  WK-DENNO          PIC 9(08).
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*システム時刻
     03  SYS-TIME          PIC 9(08).
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  KNJ-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  CAS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH1219B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH1219B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH1219B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT1.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT1       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT2.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " UPDATE= ".
         05  OUT-CNT2       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
*    03  PARA-KSYU          PIC   X(01).
*    03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KONANPOR.
     MOVE      "KONANPOR"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNJOHOF.
     MOVE      "KNJOHOL4"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNCASEF.
     MOVE      "KNCASEL4"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     INITIALIZE                   WK-AREA.
     OPEN     INPUT     KONANPOR  KNJOHOF.
     OPEN     I-O       KNCASEF.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT1  OUT-CNT2.
*
     MOVE     SPACE     TO        WK-PPDT-REC.
     INITIALIZE                   WK-PPDT-REC.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT      SYS-TIME  FROM      TIME.
*
 INIT-01.
     READ     KONANPOR
              AT END    DISPLAY NC"対象データなし" UPON CONS
                        MOVE      9         TO  END-FG
                        GO                  TO  INIT-EXIT
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
     IF       DEN-01A   NOT =    "PP"
              GO                            TO  INIT-01
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*ケースＰＤラベルレコード
     IF   DEN-01A  =     "PP"
          MOVE     SPACE           TO     WK-PPDT-REC
          INITIALIZE                      WK-PPDT-REC
          MOVE     DEN-01          TO     WK-PPDT-REC
          ADD      1               TO     CNT-KENSU
          ADD      1               TO     CNT-KENSU-D
          MOVE     PARA-JDATE      TO     JOH-F001   *>日付
          MOVE     PARA-JTIME      TO     JOH-F002   *>時刻
          MOVE     WK-PPDT09       TO     JOH-FA07   *>取引先
          MOVE     WK-PPDT10       TO     JOH-FA08   *>ルート
          MOVE     WK-PPDT06       TO     JOH-FA03   *>部門
          MOVE     WK-PPDT07       TO     JOH-FA04   *>発注日
          MOVE     WK-PPDT08       TO     JOH-FA05   *>納品日
          MOVE     WK-PPDT02       TO     JOH-FA06   *>伝ＮＯ
          MOVE     WK-PPDT04       TO     JOH-FB11   *>ＪＡＮ
          PERFORM  KNJOHOL4-READ-SEC
          IF KNJOHOL4-INV-FLG  = "INV"
             DISPLAY NC"基本情報ファイルが存在しません！"
                                                     UPON CONS
             DISPLAY NC"バッチ（日）" "=" PARA-JDATE UPON CONS
             DISPLAY NC"バッチ（時）" "=" PARA-JTIME UPON CONS
             DISPLAY NC"バッチ（取）" "=" WK-PPDT09  UPON CONS
             DISPLAY NC"ルートコード" "=" WK-PPDT10  UPON CONS
             DISPLAY NC"部門コード　" "=" WK-PPDT06  UPON CONS
             DISPLAY NC"発注日　　　" "=" WK-PPDT07  UPON CONS
             DISPLAY NC"納品日　　　" "=" WK-PPDT08  UPON CONS
             DISPLAY NC"伝票番号　　" "=" WK-PPDT02  UPON CONS
             DISPLAY NC"ＪＡＮコード" "=" WK-PPDT04  UPON CONS
             MOVE    4010          TO     PROGRAM-STATUS
             MOVE    9             TO     END-FG
             GO                    TO     MAIN-EXIT
          ELSE
             PERFORM  EDIT-SEC
          END-IF
     END-IF.
*
     READ    KONANPOR
             AT END    MOVE      9         TO  END-FG
             NOT AT END
                       ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力・更新                              *
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
     MOVE     SPACE         TO        CAS-REC.
     INITIALIZE                       CAS-REC.
*
*コーナン　ケースＰＤラベルファイル存在チェック
     MOVE     PARA-JDATE    TO        CAS-F001.
     MOVE     PARA-JTIME    TO        CAS-F002.
     MOVE     WK-PPDT09     TO        CAS-FA07.
     MOVE     WK-PPDT10     TO        CAS-FA08.
     MOVE     WK-PPDT06     TO        CAS-FA03.
     MOVE     WK-PPDT07     TO        CAS-FA04.
     MOVE     WK-PPDT08     TO        CAS-FA05.
     MOVE     WK-PPDT02     TO        CAS-FA06.
     MOVE     WK-PPDT04     TO        CAS-FB11.
     MOVE     WK-PPDT05     TO        CAS-FD05.
*2021/05/18↓
     MOVE     WK-PPDT03     TO        CAS-FD03.
*2021/05/18↑
     PERFORM  KNCASEL4-READ-SEC.
*
*　共通情報
     MOVE     JOH-F0        TO        CAS-F0.
*  伝票ヘッダ
     MOVE     JOH-FA        TO        CAS-FA.
*  伝票明細
     MOVE     JOH-FB        TO        CAS-FB.
*  付加情報
*    _番
     MOVE     JOH-FC01      TO        CAS-FC01.
*    サカタ商品コード
     MOVE     JOH-FC02      TO        CAS-FC02.
*    サカタ品単コード
     MOVE     JOH-FC03      TO        CAS-FC03.
*    システム日付
     MOVE     SYS-DATEW     TO        CAS-FC04.
*    システム時刻
     MOVE     SYS-TIME(1:6) TO        CAS-FC05.
*    登録者部門コード
     MOVE     JOH-FC06      TO        CAS-FC06.
*    担当者コード
     MOVE     JOH-FC07      TO        CAS-FC07.
*  ケースＰＤラベル情報
     MOVE     WK-PPDT-REC   TO        CAS-FD.
*
     IF       KNCASEL4-INV-FLG  = "INV"
              WRITE    CAS-REC
              ADD      1    TO        WRT-CNT
     ELSE
              REWRITE  CAS-REC
              ADD      1    TO        RWT-CNT
     END-IF.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT        TO      IN-CNT.
     MOVE      WRT-CNT       TO      OUT-CNT1.
     MOVE      RWT-CNT       TO      OUT-CNT2.
*
     DISPLAY   MSG-IN        UPON    CONS.
     DISPLAY   MSG-OUT1      UPON    CONS.
     DISPLAY   MSG-OUT2      UPON    CONS.
     DISPLAY   MSG-END       UPON    CONS.
*
     CLOSE     KONANPOR  KNJOHOF  KNCASEF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　基本情報ファイル検索
****************************************************************
 KNJOHOL4-READ-SEC         SECTION.
*
     MOVE   "KNJOHOL4-READ-SEC"     TO   S-NAME.
*
     READ   KNJOHOF
            INVALID      MOVE  "INV"   TO  KNJOHOL4-INV-FLG
            NOT  INVALID MOVE  SPACE   TO  KNJOHOL4-INV-FLG
     END-READ.
*
 KNJOHOL4-READ-EXIT.
     EXIT.
****************************************************************
*　　ケースＰＤラベルファイル検索
****************************************************************
 KNCASEL4-READ-SEC         SECTION.
*
     MOVE   "KNCASEL4-READ-SEC"     TO   S-NAME.
*
     READ   KNCASEF
            INVALID      MOVE  "INV"   TO  KNCASEL4-INV-FLG
            NOT  INVALID MOVE  SPACE   TO  KNCASEL4-INV-FLG
     END-READ.
*
 KNCASEL4-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
