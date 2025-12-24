# NJH3203B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/NJH3203B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成　　　　　　　　*
*    作成日／更新日　　　：　10/09/08                          *
*    作成者／更新者　　　：　ＮＡＶ阿部　　　　　　　　　　　　*
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*                            カインズ用                        *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*2007/09/12 １行目で振分先を決定する。                         *
*2013/03/12 受信レイアウト項目追加　　                         *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH3203B.
 AUTHOR.                NAV ABE.
 DATE-WRITTEN.          10/09/08.
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
*変換伝票データ
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEN-F46
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
*出荷場所件数マスタ
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
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２０４８　ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS 16.
         05  DEN-01A             PIC  X(01).
         05  DEN-01B             PIC  X(127).
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
*    出荷場所別件数ファイル
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
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     KHSHIRED  OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
*
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
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(06)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
*
*＜伝票ヘッダ＞
 01  WK-FH-REC.
     03  WK-FH01                  PIC       X(01).
     03  WK-FH02                  PIC       X(02).
     03  WK-FH03.
         05  WK-FH031             PIC       9(08).
         05  WK-FH032             PIC       9(01).
     03  WK-FH04.
         05  WK-FH041             PIC       9(04).
         05  WK-FH042             PIC       9(05).
     03  WK-FH05                  PIC       9(03).
     03  WK-FH06                  PIC       9(01).
     03  WK-FH07                  PIC       X(02).
     03  WK-FH08                  PIC       9(06).
     03  WK-FH09                  PIC       9(06).
     03  WK-FH10                  PIC       9(06).
     03  WK-FH11                  PIC       X(01).
     03  WK-FH12                  PIC       X(01).
     03  WK-FH13                  PIC       X(20).
     03  WK-FH14                  PIC       X(20).
     03  WK-FH15                  PIC       9(01).
     03  WK-FH16                  PIC       X(01).
     03  WK-FH17                  PIC       X(01).
     03  WK-FH18                  PIC       X(01).
     03  WK-FH19                  PIC       X(01).
     03  WK-FH20                  PIC       X(01).
     03  WK-FH21                  PIC       X(01).
     03  WK-FH22                  PIC       9(01).
     03  WK-FH23                  PIC       9(01).
     03  WK-FH24                  PIC       9(01).
     03  WK-FH25                  PIC       X(10).
     03  FILLER                   PIC       X(21).
*＜伝票明細（ＥＯＳ）＞
 01  WK-DE-REC.
     03  WK-DE01                  PIC       X(01).
     03  WK-DE02                  PIC       X(02).
     03  WK-DE03                  PIC       9(02).
     03  WK-DE04                  PIC       X(13).
     03  WK-DE05                  PIC       9(03)V9.
     03  WK-DE06                  PIC       9(04).
     03  WK-DE07                  PIC       X(02).
     03  WK-DE08                  PIC       9(05)V9.
     03  WK-DE09                  PIC       9(07)V99.
     03  WK-DE10                  PIC       9(07).
     03  WK-DE11                  PIC       9(10).
     03  WK-DE12                  PIC       9(10).
     03  WK-DE13                  PIC       9(06).
     03  WK-DE14                  PIC       X(03).
     03  WK-DE15                  PIC       X(25).
     03  WK-DE16                  PIC       X(13).
     03  WK-DE17                  PIC       X(04).
     03  WK-DE18                  PIC       9(05).
     03  WK-DE19                  PIC       X(01).
     03  FILLER                   PIC       X(01).
*＜伝票明細（ＴＯＳ）＞
 01  WK-DT-REC.
     03  WK-DT01                  PIC       X(01).
     03  WK-DT02                  PIC       X(02).
     03  WK-DT03                  PIC       9(02).
     03  WK-DT04                  PIC       X(13).
     03  WK-DT05                  PIC       9(03)V9.
     03  WK-DT06                  PIC       9(04).
     03  WK-DT07                  PIC       X(02).
     03  WK-DT08                  PIC       9(05)V9.
     03  WK-DT09                  PIC       9(07)V99.
     03  WK-DT10                  PIC       9(07).
     03  WK-DT11                  PIC       9(10).
     03  WK-DT12                  PIC       9(10).
     03  WK-DT13                  PIC       9(06).
     03  WK-DT14                  PIC       X(03).
     03  WK-DT15                  PIC       X(25).
     03  WK-DT16                  PIC       9(02).
     03  WK-DT17                  PIC       X(07).
     03  WK-DT18                  PIC       X(01).
     03  WK-DT19                  PIC       9(02).
     03  WK-DT20                  PIC       X(05).
     03  WK-DT21                  PIC       9(05).
     03  WK-DT22                  PIC       X(01).
     03  FILLER                   PIC       X(01).
*
 01  WK-AREA.
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
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH3203B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3203B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3203B".
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
     OPEN     INPUT     CVCSG001
                        SHOTBL1   MEIMS1
                        JHMRUTL1  TOKMS2.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JSMKENL1  JSMDAYL1.
     OPEN     OUTPUT    VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-FH-REC.
     INITIALIZE                   WK-FH-REC.
     MOVE     SPACE     TO        WK-DE-REC.
     INITIALIZE                   WK-DE-REC.
     MOVE     SPACE     TO        WK-DT-REC.
     INITIALIZE                   WK-DT-REC.
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
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >         16
*
*伝票ヘッダ処理
        IF    DEN-01A(IDX)  =     "B"
*             出荷場所件数マスタ出力
              IF   CNT-MAISU     >    ZERO
                   PERFORM  JSMKENL1-WRT-SEC
              END-IF
              MOVE      SPACE         TO    WK-FH-REC
              INITIALIZE                    WK-FH-REC
              MOVE      DEN-01(IDX)   TO    WK-FH-REC
              ADD       1        TO   CNT-MAISU
              MOVE      ZERO     TO   CNT-KENSU-D
              MOVE      ZERO     TO   CNT-GYO
        END-IF
*明細行
        IF    DEN-01A(IDX)  =     "D"
              MOVE      SPACE         TO    WK-DE-REC
              INITIALIZE                    WK-DE-REC
              MOVE      DEN-01(IDX)   TO    WK-DE-REC
              MOVE      SPACE         TO    WK-DT-REC
              INITIALIZE                    WK-DT-REC
              MOVE      DEN-01(IDX)   TO    WK-DT-REC
              ADD       1        TO   CNT-KENSU
              ADD       1        TO   CNT-KENSU-D
        END-IF
*
        IF    DEN-01A(IDX)  =     "D"
              PERFORM                       EDIT-SEC
        END-IF
     END-PERFORM.
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
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
     MOVE     SPACE         TO        HEN-REC.
     INITIALIZE                       HEN-REC.
*各量販店領域転送
*    伝票区分
     MOVE     WK-FH07       TO        HEN-A01.
*    分類コード
     MOVE     WK-FH05       TO        HEN-A02.
*    発注者名
     MOVE     WK-FH13       TO        HEN-A04.
*    取引先名称
     MOVE     "ｶ)ｻｶﾀﾉﾀﾈ"    TO        HEN-A05.
*    相手商品コード　
     MOVE     WK-DE04       TO        HEN-A06.
*    相手ＪＡＮコード
     IF  WK-DE02   =   "01"
         MOVE WK-DE04       TO        HEN-A07
     END-IF.
*    商品名１
     MOVE     WK-DE15       TO        HEN-A08.
*    商品名２
     MOVE     SPACE         TO        HEN-A09.
*    数量
     MOVE     WK-DE08       TO        HEN-A10.
*    原価単価
     MOVE     WK-DE09       TO        HEN-A11.
*    売価単価
     MOVE     WK-DE10       TO        HEN-A12.
*    原価金額
     MOVE     WK-DE11       TO        HEN-A13.
*    売価金額
     MOVE     WK-DE12       TO        HEN-A14.
*    配送ルート
     MOVE     WK-FH16       TO        HEN-A15.
*    伝票枚数
     MOVE     ZERO          TO        HEN-A16.
*    法人コード　
     MOVE     WK-FH041      TO        HEN-A17.
*    メーカー発行区分
     IF       WK-FH11   =   SPACE
         MOVE      0        TO        HEN-A18
     ELSE
         MOVE      WK-FH11  TO        HEN-A18
     END-IF.
*    発注区分
     MOVE     WK-FH15       TO        HEN-A19.
*    配送パターン
     MOVE     WK-FH16       TO        HEN-A20.
*    伝票発行区分
     MOVE     WK-FH17       TO        HEN-A21.
*    入数
     MOVE     WK-DE05       TO        HEN-A22.
*    ケース数
     MOVE     WK-DE06       TO        HEN-A23.
*    発注単位区分
     MOVE     WK-DE07       TO        HEN-A24.
*    外注
     MOVE     WK-DE13       TO        HEN-A25.
*    取引先品番
     MOVE     WK-DE18       TO        HEN-A26.
*    追加区分
     MOVE     WK-DE19       TO        HEN-A27.
     IF       WK-DE02  NOT =     "01"
*        色コード
         MOVE WK-DT16       TO        HEN-A28
*        色名称
         MOVE WK-DT17       TO        HEN-A29
*        サイズコード
         MOVE WK-DT19       TO        HEN-A30
*        サイズ名
         MOVE WK-DT20       TO        HEN-A31
     END-IF.
*    商品区分
     MOVE     WK-FH06       TO        HEN-A32.
*    データ区分
     MOVE     WK-DE02       TO        HEN-A33.
*2013/03/12 NAV ST 項目追加
*    共配区分
     MOVE     WK-FH18       TO        HEN-A35.
*    共配センター区分
     MOVE     WK-FH19       TO        HEN-A36.
*    売場コード
     MOVE     WK-FH20       TO        HEN-A37.
*    納入センターＣＤ
     MOVE     WK-FH21       TO        HEN-A38.
*    ＥＤＩ区分
     MOVE     WK-FH22       TO        HEN-A39.
*    納品形態
     MOVE     WK-FH23       TO        HEN-A40.
*    税取扱区分
     MOVE     WK-FH24       TO        HEN-A41.
*    納入センター名
     MOVE     WK-FH25       TO        HEN-A42.
*2013/03/12 NAV ED 項目追加
*
     PERFORM  TENSO-SEC.
*
     WRITE    HEN-REC.
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*取引先コード
     MOVE        WK-FH10   TO        HEN-F01.
     MOVE        WK-FH10   TO        WK-TOKCD.
*伝票ナンバー
     MOVE        WK-FH03   TO        HEN-F02.
     MOVE        HEN-F02   TO        HEN-F23.
*行■
     MOVE        WK-DE03   TO        HEN-F03.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE        WK-FH042  TO        HEN-F07.
*  商品変換テーブル検索
     MOVE        SPACE     TO        TBL-REC.
     INITIALIZE                      TBL-REC.
     MOVE        WK-FH10   TO        TBL-F01.
     MOVE        HEN-A06   TO        TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*振分ルート取得 2002/03/06  追加
     IF  HEN-F02  NOT =  WK-DENNO
         MOVE   TBL-F04    TO        WK-RUTO
         MOVE   HEN-F02    TO        WK-DENNO
     END-IF.
*---------------------------*
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        WK-FH08   TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F111
     ELSE
         MOVE    ZERO           TO   HEN-F111
     END-IF.
*納品日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        WK-FH09   TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F112
     ELSE
         MOVE    ZERO           TO   HEN-F112
     END-IF.
*分類（部門）
     MOVE        HEN-A02   TO        HEN-F12.
*商品区分　　
     MOVE        SPACE     TO        HEN-F131.
*伝票区分　　
     MOVE        HEN-A01   TO        HEN-F132.
*伝発区分
     MOVE        9         TO        HEN-F134.
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
         MOVE    HEN-A08   TO        HEN-F142
       NOT INVALID
         MOVE    HEN-A08   TO        HEN-F142
     END-READ.
*数量
     MOVE        HEN-A10   TO        HEN-F15.
*単
     MOVE       "1"        TO        HEN-F16.
*原価単価
     MOVE        HEN-A11   TO        HEN-F172.
*売価単価
     MOVE        HEN-A12   TO        HEN-F173.
*原価金額
     MOVE        HEN-A13   TO        HEN-F181.
*売価金額
     MOVE        HEN-A14   TO        HEN-F182.
*備考
     MOVE        SPACE     TO        HEN-F22.
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    WK-FH10       TO    TOK-F01
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
*相手商品コード
     MOVE        HEN-A07   TO        HEN-F25.
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
*ＷＳ■
     MOVE        1         TO        HEN-F28.
*店舗名（カナ）
*****MOVE        WK-FH14   TO        HEN-F30.
*## 1999/11/08 NAV T.T START ##*
     MOVE        WK-FH14   TO        HEN-A34.
*## 1999/11/08 NAV T.T END   ##*
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*振分倉庫コード
*    ルート条件マスタ検索
*****MOVE     ZERO         TO        INV-RUT.
*****MOVE     SPACE        TO        RUT-REC.
*****INITIALIZE                      RUT-REC.
*****MOVE     WK-FH10      TO        RUT-F01.
*****MOVE     SPACE        TO        RUT-F02.
*****MOVE     HEN-A15      TO        RUT-F03  HEN-F42.
*****READ     JHMRUTL1
*****    INVALID
*****      MOVE  1         TO        INV-RUT
*****      MOVE  TOK-F81   TO        HEN-F48
*****    NOT INVALID
*****      MOVE  RUT-F05   TO        HEN-F48
*****END-READ.
*ルートＣＤのセット 2007/09/12 追加
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        HEN-F48 KEN-F04 HEN-F42
     ELSE
           MOVE  WK-RUTO   TO        HEN-F48 KEN-F04 HEN-F42
     END-IF.
*****
*---------------------------*
*_番
     MOVE        TBL-F08   TO        HEN-F49.
*訂正前数量
     MOVE        HEN-A10   TO        HEN-F50.
*修正原価単価
     MOVE        HEN-A11   TO        HEN-F512.
*修正売価単価
     MOVE        HEN-A12   TO        HEN-F513.
*修正原価金額
     MOVE        HEN-A13   TO        HEN-F521.
*修正売価金額
     MOVE        HEN-A14   TO        HEN-F522.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-FH10       TO        KEN-F03.
*****IF      INV-RUT       =         ZERO
*****        MOVE  RUT-F05       TO    KEN-F04
*****ELSE
*****        MOVE  TOK-F81       TO    KEN-F04
*****END-IF.
*ルートＣＤのセット 2007/09/12
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        KEN-F04
     ELSE
           MOVE  WK-RUTO   TO        KEN-F04
     END-IF.
*****
*---------------------------*
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
     MOVE    WK-FH10       TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
*ルートＣＤのセット 2007/09/12
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        KEN-F04
     ELSE
           MOVE  WK-RUTO   TO        KEN-F04
     END-IF.
*****
*---------------------------*
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
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     CVCSG001  JHSHENL1
               SHOTBL1   MEIMS1
               JHMRUTL1  TOKMS2
               JSMKENL1  JSMDAYL1.
*
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
***************DISPLAY "AAA" UPON CONS
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
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-FH10       TO        TJS-F03.
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
     MOVE    WK-FH10       TO        TJS-F03.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     WRITE   TJS-REC.
     GO      TO   JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
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
     MOVE      WK-FH10            TO    VLD-F13.
***  DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
***  DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
***  DISPLAY "WK-TOKCD   = " WK-TOKCD   UPON CONS.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
