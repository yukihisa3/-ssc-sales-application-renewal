****************************************************************
*    ※新ＥＤＩシステムの構築に伴う変更 2010/09/09             *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　新規取引先オンライン（ダイユー）　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　受信データ編集　　　　　　　　　　*
*    作成日／更新日　　　：　08/04/09                          *
*    作成者／更新者　　　：　ＮＡＶ　阿部　　　　　　　　　　　*
*    処理概要　　　　　　：　ダイユー受信データを読み、当日売上*
*                            ファイルと発注基本情報ファイルを  *
*                            出力する　　　                    *
*    作成日／更新日　　　：　10/09/09                          *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　新ＥＤＩシステムへ移行の為　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH7350B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/09.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*当日売上ファイル
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY                 HEN-F46
                                            HEN-F47   HEN-F01
                                            HEN-F02   HEN-F03
                        FILE  STATUS   IS   HEN-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*受信件数マスタ
     SELECT   JSMKENL1  ASSIGN    TO        DA-01-VI-JSMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*発注基本情報ファイル　　　
     SELECT   DYJOHOF   ASSIGN    TO        DA-01-VI-DYJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F01   JOH-F02
                                            JOH-F03   JOH-F04
                                            JOH-F05   JOH-F06
                                            JOH-F07   JOH-F08
                        FILE  STATUS   IS   JOH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01.
         05  DEN-01A             PIC  X(02).
         05  DEN-01B             PIC  X(3139).
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
******************************************************************
*    受信件数マスタ　　　　
******************************************************************
 FD  JSMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    当日売上ファイル　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     DYSHIRED  OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
******************************************************************
*    ＶＬＤ５００
******************************************************************
 FD  VLD500.
 01  VLD-REC.
     03  VLD-F01           PIC  X(02).
     03  VLD-F02           PIC  9(03).
     03  VLD-F03           PIC  X(02).
     03  VLD-F04           PIC  X(08).
     03  VLD-F05           PIC  9(06).
     03  VLD-F06           PIC  9(01).
     03  VLD-F07           PIC  X(02).
     03  VLD-F08           PIC  9(02).
     03  VLD-F09           PIC  9(02).
     03  VLD-F10           PIC  9(04).
     03  VLD-F11           PIC  9(08).
     03  VLD-F12           PIC  9(04).
     03  VLD-F13           PIC  9(08).
     03  FILLER            PIC  X(48).
*
******************************************************************
*    発注基本情報ファイル　　　
******************************************************************
 FD  DYJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DYJOHOF   OF        XFDLIB
              JOINING   JOH  AS   PREFIX.
******************************************************************
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*発注データ（ヘッダ）
     COPY     DYHACHDF  OF   XFDLIB   JOINING   RHD    PREFIX.
*発注データ（明細）
     COPY     DYHACMEF  OF   XFDLIB   JOINING   RME    PREFIX.
*発注データ（ヘッダ）チェック用
     COPY     DYHACHDF  OF   XFDLIB   JOINING   CHK    PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  SK-FLG                  PIC  X(01)     VALUE  SPACE.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SK-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
*ルート変換
 01  WK-DENPYO               PIC  9(09)     VALUE  ZERO.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
 01  WK-TORICD               PIC  9(08)     VALUE  ZERO.
*
 01  WK-JANCD.
     03  WK-JANCD1         PIC X(13).
     03  FILLER            PIC X(01).
*
 01  WK-AREA.
     03  WK-GYO            PIC 9(02).
     03  WK-DENNO          PIC 9(08).
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
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
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH7350B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH7350B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH7350B".
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
     03  MSG-SKIP.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " SKIP  = ".
         05  SKP-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
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
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSHENL1.
     MOVE      "JHSHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMKENL1.
     MOVE      "JSMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMDAYL1.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   VLD500.
     MOVE      "VLD500  "   TO   AB-FILE.
     MOVE      VLD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYJOHOF.
     MOVE      "DYJOHOL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     DISPLAY "HEN-F02 = " HEN-F02 UPON CONS.
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
     OPEN     INPUT     CVCSG001
                        SHOTBL1   MEIMS1
                        JHMRUTL1  TOKMS2.
     OPEN     EXTEND    JHSHENL1  DYJOHOF.
     OPEN     I-O       JSMKENL1  JSMDAYL1.
     OPEN     OUTPUT    VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    MOVE     SPACE     TO        WK-FLHD-REC.
*    INITIALIZE                   WK-FLHD-REC.
     MOVE     SPACE     TO        RHD-REC.
     INITIALIZE                   RHD-REC.
     MOVE     SPACE     TO        RME-REC.
     INITIALIZE                   RME-REC.
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
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
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
*店別・総量区分”３”読飛判定
     IF    DEN-01A       =     "HD"
           MOVE      SPACE         TO    SK-FLG
           MOVE      DEN-01        TO    CHK-REC
           IF  CHK-H12   =   3
               ADD     1          TO    SK-CNT
               MOVE    9          TO    SK-FLG
           END-IF
     END-IF.
*
*伝票ヘッダ処理
     IF    SK-FLG        =     SPACE
        IF    DEN-01A       =     "HD"
*             出荷場所件数マスタ出力
              IF   CNT-MAISU     >    ZERO
                   PERFORM  JSMKENL1-WRT-SEC
              END-IF
              MOVE      SPACE         TO    RHD-REC
              INITIALIZE                    RHD-REC
              MOVE      DEN-01        TO    RHD-REC
              ADD       1        TO   CNT-MAISU
              MOVE      ZERO     TO   CNT-KENSU-D

              MOVE      ZERO     TO   CNT-GYO
        END-IF
*明細行
        IF    DEN-01A       =     "DT"
              MOVE      SPACE         TO    RME-REC
              INITIALIZE                    RME-REC
              MOVE      DEN-01        TO    RME-REC
              ADD       1        TO   CNT-KENSU
              ADD       1        TO   CNT-KENSU-D
        END-IF
*
        EVALUATE   DEN-01A
              WHEN "HD"
                    PERFORM   DYHED-TENSO-SEC
              WHEN "DT"
                    PERFORM   DYMEI-TENSO-SEC
        END-EVALUATE
*
        PERFORM     DYJOH-TENSO-SEC
     END-IF.
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　受信ヘッダ転送                                    *
****************************************************************
 DYHED-TENSO-SEC             SECTION.
*
     MOVE    "DYHED-TENSO-SEC"   TO        S-NAME.
     MOVE     SPACE         TO        HEN-REC  JOH-REC.
     INITIALIZE                       HEN-REC  JOH-REC.
*取引先コード
     MOVE        1225      TO        HEN-F01  HEN-A15  WK-TORICD.
*****MOVE        RHD-H04   TO        HEN-F01  HEN-A15.
*伝票番号　　
     MOVE        RHD-H03   TO        HEN-F02  HEN-A02.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE        RHD-H17   TO        HEN-F07  HEN-A14.
*注文日
     MOVE        RHD-H06   TO        HEN-F111 HEN-A03.
*納品日
     MOVE        RHD-H08   TO        HEN-F112 HEN-A04.
*分類（部門）
     MOVE        RHD-H16   TO        HEN-F12.
*伝票区分　　
     MOVE        RHD-H11   TO        HEN-F132 HEN-A07.
*単
     MOVE       "1"        TO        HEN-F16.
*指定伝票番号
     MOVE        RHD-H03   TO        HEN-F23.
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
*********MOVE    RHD-H04       TO    TOK-F01
         MOVE    WK-TORICD     TO    TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE     TO    TOK-REC
               INITIALIZE            TOK-REC
         END-READ
         MOVE    TOK-F52   TO        HEN-F24
         MOVE    1         TO        FLG-TOK
     ELSE
         MOVE    TOK-F52   TO        HEN-F24
     END-IF.
*伝票発行区分
     MOVE        9         TO        HEN-F272.
*オンライン区分
     MOVE        1         TO        HEN-F274.
*エントリー区分
     MOVE        1         TO        HEN-F275.
*付番区分
     MOVE        9         TO        HEN-F276.
*量販店区分
     MOVE       "A"        TO        HEN-F278.
*ＷＳ_
     MOVE        1         TO        HEN-F28.
*店舗名カナ
     MOVE        RHD-H18   TO        HEN-F30 HEN-A18.
*ルート
*****MOVE        SPACE     TO        HEN-F42.
     MOVE        WK-RUTO   TO        HEN-F42.
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*タグ
     MOVE        RHD-H01   TO        HEN-A01.
*伝票タイプ（店舗コード)
     MOVE        RHD-H17   TO        HEN-A05.
*発注日
     MOVE        ZERO      TO        HEN-A08.
*納品日
     MOVE        ZERO      TO        HEN-A09.
*取引先コード
     MOVE        ZERO      TO        HEN-A10.
*部門名称（カナ）
     MOVE        ZERO      TO        HEN-A11.
*部門名称（漢字）
     MOVE        SPACE     TO        HEN-A12.
*部門コード
     MOVE        RHD-H16   TO        HEN-A13.
*仕入先名称（カナ）
     MOVE        RHD-H21  TO         HEN-A16.
*仕入先名称（漢字）
*    MOVE        RHD-H22(1:20)  TO   HEN-A17.
*店舗名称（漢字）
*    MOVE        RHD-H19(1:20)  TO   HEN-A19.
*送信先コード
     MOVE        SPACE     TO        HEN-A20.
*仕入先ＴＥＬ
     MOVE        SPACE     TO        HEN-A21.
 DYHED-TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　受信明細転送                                      *
****************************************************************
 DYMEI-TENSO-SEC             SECTION.
*
     MOVE    "DYMEI-TENSO-SEC"   TO        S-NAME.
*
*行_
     MOVE    RME-M03       TO        HEN-F03 HEN-A03.
*  商品変換テーブル検索
     MOVE    RHD-H04       TO        TBL-F01.
     MOVE    RME-M02(1:13) TO        TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*振分ルート取得
     IF  HEN-F02  NOT =  WK-DENPYO
         MOVE   TBL-F04    TO        WK-RUTO
         MOVE   HEN-F02    TO        WK-DENPYO
     END-IF.
*自社商品コード
     MOVE        TBL-F031  TO        HEN-F1411.
*自社商品単品コード
     MOVE        TBL-F032  TO        HEN-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE        TBL-F031  TO        MEI-F011.
     MOVE        TBL-F032  TO        MEI-F012.
     READ    MEIMS1
       INVALID
*        商品名
         MOVE    RME-M05(1:15)   TO  HEN-F1421
         MOVE    RME-M05(16:10)  TO  HEN-F1422
       NOT INVALID
         MOVE    RME-M05(1:15)   TO  HEN-F1421
         MOVE    RME-M05(16:10)  TO  HEN-F1422
     END-READ.
*数量
     COMPUTE     HEN-F15    =  RME-M07  /  100.
     MOVE        HEN-F15   TO        HEN-A28.
*原価単価
     MOVE        RME-M12   TO        HEN-A31.
     COMPUTE     HEN-F172  =      RME-M12  /  100.
*売価単価
**** COMPUTE     HEN-F173  =      RME-M13  /  100.
**** MOVE        HEN-F173  TO        HEN-A32.
     MOVE        RME-M13   TO     HEN-F173.
     MOVE        RME-M13   TO     HEN-A32.
*原価金額
     COMPUTE     HEN-F181 = HEN-F15 * HEN-F172.
*売価金額
     COMPUTE     HEN-F182 = HEN-F15 * HEN-F173
*指定商品コード
*****MOVE        RME-M02   TO        HEN-F25.
*****MOVE        RME-M02(1:13) TO    WK-JANCD
*****MOVE        WK-JANCD1     TO    HEN-F25.
     MOVE        RME-M02(1:13) TO    HEN-F25.
*振分倉庫コード
*    ルート条件マスタ検索
*    MOVE     ZERO         TO        INV-RUT.
*    MOVE     SPACE        TO        RUT-REC.
*    INITIALIZE                      RUT-REC.
*    MOVE     RHD-H04      TO        RUT-F01.
*    MOVE     SPACE        TO        RUT-F02.
*    MOVE     SPACE        TO        RUT-F03.
*    READ     JHMRUTL1
*        INVALID
*          MOVE  1               TO    INV-RUT
*          MOVE  TOK-F81         TO    HEN-F48
*        NOT INVALID
*          MOVE  RUT-F05         TO    HEN-F48
*    END-READ.
*ルートへ振分先ＣＤセット
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        HEN-F48  HEN-F42(1:2)
***********MOVE  RHD-H11   TO        HEN-F42(4:2)
     ELSE
           MOVE  WK-RUTO   TO        HEN-F48  HEN-F42(1:2)
***********MOVE  RHD-H11   TO        HEN-F42(4:2)
     END-IF.
*伝票区分名とルート（４：５）にセット
     EVALUATE    RHD-H11
         WHEN    0     MOVE "-ﾃｲﾊﾞﾝ" TO    HEN-F42(3:6)
         WHEN    1     MOVE "ｺﾞｳﾄﾞｳ" TO    HEN-F42(3:6)
         WHEN    4     MOVE "-ﾌﾘｶｴ " TO    HEN-F42(3:6)
         WHEN    5     MOVE "-ﾄｸﾊﾞｲ" TO    HEN-F42(3:6)
         WHEN    7     MOVE "ﾀﾞｲﾚｸﾄ" TO    HEN-F42(3:6)
         WHEN    OTHER MOVE SPACE    TO    HEN-F42(3:6)
     END-EVALUATE.
*_番
*****MOVE        SPACE           TO    HEN-F49.
*2010/10/05 NAV 修正 START
     MOVE        TBL-F08         TO    HEN-F49.
*2010/10/05 NAV 修正 END
*訂正前数量
     MOVE        HEN-F15         TO    HEN-F50.
*訂正前原価単価
     MOVE        HEN-F172        TO    HEN-F512.
*訂正前売価単価
     MOVE        RME-M13         TO    HEN-F513.
*訂正前原価金額
     MOVE        HEN-F181        TO    HEN-F521.
*訂正前売価金額
     MOVE        HEN-F182        TO    HEN-F522.
*タグ
     MOVE        RME-M01         TO    HEN-A22.
*JANCD
     MOVE        RME-M02         TO    HEN-A23.
*商品名（カナ）
     MOVE        RME-M05         TO    HEN-A25.
*商品名（漢字）
*    MOVE        RME-M06         TO    HEN-A26.
*規格名称（カナ）
     MOVE        SPACE           TO    HEN-A27.
*規格名称（漢字）
*    MOVE        SPACE           TO    HEN-A28.
*単位
     MOVE        ZERO            TO    HEN-A29.
*入数
     MOVE        RME-M09         TO    HEN-A31.
*発注数量（納品数量元値）
     MOVE        RME-M07         TO    HEN-A32.
*発注単位
     MOVE        ZERO            TO    HEN-A33.
*原価金額
     MOVE        RME-M10         TO    HEN-A34.
*売価金額
     MOVE        RME-M11         TO    HEN-A35.
*
     WRITE    HEN-REC.
*    ADD      1             TO   WRT-CNT.
 DYMEI-TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　発注基本情報ファイル出力                          *
****************************************************************
 DYJOH-TENSO-SEC             SECTION.
*
     MOVE    "DYMEI-TENSO-SEC"   TO        S-NAME.
*受信日付
     MOVE     HEN-F46      TO        JOH-F01.
*受信時刻
     MOVE     HEN-F47      TO        JOH-F02.
*取引先コード
     MOVE     HEN-F01      TO        JOH-F03.
*倉庫コード　
     MOVE     HEN-F48      TO        JOH-F04.
*店舗コード　
     MOVE    HEN-F07       TO        JOH-F05.
*伝票番号　　
     MOVE    HEN-F02       TO        JOH-F06.
*行番号　　　
     MOVE    HEN-F03       TO        JOH-F07.
*発注日　　　
     MOVE    RHD-H06       TO        JOH-F15.
*納品日　　　
     MOVE    RHD-H08       TO        JOH-F08  JOH-F16.
*分類コード　
     MOVE    HEN-F12       TO        JOH-F13.
*伝票区分　　
     MOVE    HEN-F132      TO        JOH-F14.
*ヘッダレコード
     MOVE    RHD-REC       TO        JOH-HD00.
*
     IF      DEN-01A   =   "DT"
*明細レコード
             MOVE    RME-REC       TO        JOH-ME00
*
*************MOVE    RME-M02(1:13) TO        WK-JANCD
*************MOVE    WK-JANCD1     TO        JOH-M02
*
             WRITE   JOH-REC
             ADD     1             TO        WRT-CNT
     END-IF.
 DYJOH-TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
*****MOVE    RHD-H04       TO        KEN-F03.
     MOVE    WK-TORICD     TO        KEN-F03.
*****IF      INV-RUT       =         ZERO
*            MOVE  RUT-F05     TO    KEN-F04
*    ELSE
*            MOVE  TOK-F81     TO    KEN-F04
*****END-IF.
*ルートＣＤのセット
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        KEN-F04
     ELSE
           MOVE  WK-RUTO   TO        KEN-F04
     END-IF.
*****
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-010
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
*****MOVE    RHD-H04       TO        KEN-F03.
     MOVE    WK-TORICD     TO        KEN-F03.
*****IF      INV-RUT       =         ZERO
*            MOVE  RUT-F05     TO    KEN-F04
*    ELSE
*            MOVE  TOK-F81     TO    KEN-F04
*****END-IF.
*ルートＣＤのセット
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        KEN-F04
     ELSE
           MOVE  WK-RUTO   TO        KEN-F04
     END-IF.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JSMKENL1-WRT-EXIT.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     ADD     CNT-KENSU-D   TO        KEN-F10.
     ADD     1             TO        KEN-F11.
     REWRITE KEN-REC.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        CNT-MAISU     >    ZERO
*              出荷場所件数マスタ出力
               PERFORM  JSMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      SK-CNT    TO      SKP-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-SKIP  UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     CVCSG001  JHSHENL1  DYJOHOF
               SHOTBL1   MEIMS1
               JHMRUTL1  TOKMS2
               JSMKENL1  JSMDAYL1.
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
     CLOSE     VLD500.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL1-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
*****MOVE    RHD-H04       TO        TJS-F03.
     MOVE    WK-TORICD     TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
*****MOVE    RHD-H04       TO        TJS-F03.
     MOVE    WK-TORICD     TO        TJS-F03.
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     WRITE   TJS-REC.
     GO      TO   JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT-SEC       SECTION.
*
     MOVE   "VLD500-OUTPUT-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
*****MOVE      RHD-H04            TO    VLD-F13.
     MOVE      WK-TORICD          TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
