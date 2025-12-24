****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成　　　　　　　　*
*    作成日／更新日　　　：　99/09/01                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*                            島忠用                            *
*                            2000/05/19                        *
*                            ルート条件マスタ追加              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH1601B.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/01.
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
     SELECT   JHMKENL1  ASSIGN    TO        DA-01-VI-JHMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*当日スケジュールマスタ
     SELECT   JHMTJSL1  ASSIGN    TO        DA-01-VI-JHMTJSL1
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
*    受信データ　ＲＬ＝　２５６　ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS 4.
         05  DEN-01A             PIC  X(01).
         05  DEN-01B             PIC  9(02).
         05  DEN-01C             PIC  X(61).
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
 FD  JHMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JHMTJSL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMTJSF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SMSHIRED  OF        XFDLIB
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
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
*
*ＣＶＣＳデータ退避
 01  WK-DEN-REC.
     03  WK-DEN1.
         05  WK-DEN1-01      PIC  X(01).
         05  WK-DEN1-02      PIC  9(02).
         05  WK-DEN1-03      PIC  9(12).
         05  WK-DEN1-04      PIC  9(12).
         05  WK-DEN1-05.
             07  WK-DEN1-051 PIC  9(06).
             07  WK-DEN1-052 PIC  9(02).
         05  WK-DEN1-06      PIC  9(08).
         05  WK-DEN1-07.
             07  WK-DEN1-071 PIC  9(06).
             07  WK-DEN1-072 PIC  9(02).
         05  WK-DEN1-08      PIC  9(03).
         05  WK-DEN1-09      PIC  9(06).
         05  WK-DEN1-10      PIC  9(05).
         05  WK-DEN1-11      PIC  9(05).
     03  WK-DEN2.
         05  WK-DEN2-01      PIC  X(01).
         05  WK-DEN2-02      PIC  9(02).
         05  WK-DEN2-03      PIC  9(02).
         05  WK-DEN2-04      PIC  X(25).
         05  WK-DEN2-05      PIC  X(14).
         05  WK-DEN2-06      PIC  X(20).
     03  WK-DEN3.
         05  WK-DEN3-01      PIC  X(01).
         05  WK-DEN3-02      PIC  9(02).
         05  WK-DEN3-03      PIC  9(07).
         05  WK-DEN3-04      PIC  9(02).
         05  WK-DEN3-05      PIC  9(02).
         05  WK-DEN3-06      PIC  9(02).
         05  WK-DEN3-07.
             07  WK-DEN3-071 PIC  X(03).
             07  WK-DEN3-072 PIC  9(05).
         05  WK-DEN3-08      PIC  X(02).
         05  WK-DEN3-09      PIC  9(06).
         05  WK-DEN3-10      PIC  9(06).
         05  WK-DEN3-11      PIC  X(20).
         05  WK-DEN3-12      PIC  9(04).
         05  WK-DEN3-13      PIC  X(01).
         05  WK-DEN3-14      PIC  9(01).
     03  WK-DEN4.
         05  WK-DEN4-01      PIC  X(01).
         05  WK-DEN4-02      PIC  9(02).
         05  WK-DEN4-03      PIC  9(01).
         05  WK-DEN4-04      PIC  9(07).
         05  WK-DEN4-05      PIC  9(05).
         05  WK-DEN4-06      PIC  9(06)V99.
         05  WK-DEN4-07      PIC  9(06).
         05  WK-DEN4-08      PIC  X(30).
         05  WK-DEN4-09      PIC  9(01).
         05  WK-DEN4-10      PIC  X(03).
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
         05  ST-PG          PIC   X(08)  VALUE "SJH1601B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH1601B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH1601B".
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
     03  LINK-IN-KBN       PIC X(01).
     03  LINK-IN-YMD6      PIC 9(06).
     03  LINK-IN-YMD8      PIC 9(08).
     03  LINK-OUT-RET      PIC X(01).
     03  LINK-OUT-YMD8     PIC 9(08).
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
                        PROCEDURE   JHMKENL1.
     MOVE      "JHMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMTJSL1.
     MOVE      "JHMTJSL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
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
 FILEERR-SEC9           SECTION.
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
     OPEN     I-O       JHMKENL1  JHMTJSL1.
     OPEN     OUTPUT    VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DEN-REC.
     INITIALIZE                   WK-DEN-REC.
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
              UNTIL     IDX      >         4
*
*ファイルヘッダ処理
        IF    DEN-01A(IDX)  =     "K"
              MOVE   DEN-01(IDX)  TO   WK-DEN1
        END-IF
*ファイルヘッダオプション処理
        IF    DEN-01A(IDX)  =     "J"
              MOVE   DEN-01(IDX)  TO   WK-DEN2
        END-IF
*伝票ヘッダ処理
        IF    DEN-01A(IDX)  =     "L"
*             出荷場所件数マスタ出力
              IF   CNT-MAISU     >    ZERO
                   IF   WK-DEN3-03    NOT =     ZERO
                        PERFORM  JHMKENL1-WRT-SEC
                   END-IF
              END-IF
              MOVE      DEN-01(IDX)   TO    WK-DEN3
              IF   WK-DEN3-03    NOT =     ZERO
                   ADD  1        TO   CNT-MAISU
              END-IF
              MOVE      ZERO     TO   CNT-KENSU-D
        END-IF
*明細行
        IF    DEN-01A(IDX)  =     "N"
              MOVE      DEN-01(IDX)   TO    WK-DEN4
              IF   WK-DEN3-03    NOT =     ZERO
                   ADD      1    TO   CNT-KENSU
                   ADD      1    TO   CNT-KENSU-D
              END-IF
        END-IF
*
        IF    DEN-01A(IDX)  =     "N"
              IF   WK-DEN3-03    NOT =     ZERO
                   PERFORM       EDIT-SEC
              END-IF
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
     MOVE    "EDIT-SEC"     TO   S-NAME.
     MOVE     SPACE         TO   HEN-REC.
     INITIALIZE                  HEN-REC.
*各量販店領域転送
*    取引先コード
     MOVE     "000"         TO   HEN-A01(1:3).
     IF       WK-DEN3-072   NOT =      ZERO
         MOVE WK-DEN3-072   TO   HEN-A01(4:5)
     ELSE
         MOVE 72028         TO   HEN-A01(4:5)
     END-IF.
*    伝票_
     MOVE     WK-DEN3-03    TO   HEN-A02.
*    行_
     MOVE     WK-DEN4-03    TO   HEN-A03.
*    店コード
     MOVE     WK-DEN3-04    TO   HEN-A04.
*    部門
     MOVE     WK-DEN3-05    TO   HEN-A05.
*    伝区
     MOVE     WK-DEN3-06    TO   HEN-A06.
*    仕入先
     MOVE     WK-DEN3-07    TO   HEN-A07.
*    発注日
     MOVE     WK-DEN3-09    TO   HEN-A08.
*    納品日
     MOVE     WK-DEN3-10    TO   HEN-A09.
*    特売月日
     MOVE     WK-DEN3-12    TO   HEN-A10.
*    担当営業所
     MOVE     WK-DEN3-13    TO   HEN-A11.
*    発注企業名
     MOVE     WK-DEN2-05    TO   HEN-A12.
*    店名
     MOVE     WK-DEN3-11    TO   HEN-A13.
*    ロケーションコード
     MOVE     WK-DEN3-08    TO   HEN-A14.
*    商品コード
     MOVE     WK-DEN4-04    TO   HEN-A15.
*    単位
     MOVE     WK-DEN4-10    TO   HEN-A16.
*    商品名
     MOVE     WK-DEN4-08    TO   HEN-A17.
*    発注数
     MOVE     WK-DEN4-05    TO   HEN-A18.
*    原価
     MOVE     WK-DEN4-06    TO   HEN-A19.
*    売価
     MOVE     WK-DEN4-07    TO   HEN-A20.
*    伝票枚数
     MOVE     ZERO          TO   HEN-A21.
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
     MOVE        HEN-A01   TO        HEN-F01.
*伝票ナンバー
     MOVE        HEN-A02   TO        HEN-F02.
     MOVE        HEN-A02   TO        HEN-F23.
*行_
     MOVE        HEN-A03   TO        HEN-F03.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE        HEN-A04   TO        HEN-F07.
*  商品変換テーブル検索
     MOVE        HEN-A01   TO        TBL-F01.
     MOVE        HEN-A15   TO        TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
*****COMPUTE     HEN-F111  = WK-NEN * 10000 + HEN-A08.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A08   TO        LINK-IN-YMD6.
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
*****COMPUTE     HEN-F112  = WK-NEN * 10000 + HEN-A09.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A09   TO        LINK-IN-YMD6.
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
     MOVE        HEN-F112  TO        HEN-F113.
*分類（部門）
     MOVE        HEN-A05   TO        HEN-F12.
*商品区分　　
     MOVE        SPACE     TO        HEN-F131.
*伝票区分　　
     MOVE        HEN-A06   TO        HEN-F132.
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
         MOVE    HEN-A17(1:15)  TO   HEN-F1421
         MOVE    HEN-A17(16:15) TO   HEN-F1422
       NOT INVALID
         MOVE    HEN-A17(1:15)  TO   HEN-F1421
         MOVE    HEN-A17(16:15) TO   HEN-F1422
     END-READ.
*数量
     MOVE        HEN-A18   TO        HEN-F15.
*単
     MOVE       "1"        TO        HEN-F16.
*原価単価
     MOVE        HEN-A19   TO        HEN-F172.
*売価単価
     MOVE        HEN-A20   TO        HEN-F173.
*原価金額
     COMPUTE     HEN-F181  =     HEN-A18    *    HEN-A19.
*売価金額
     COMPUTE     HEN-F182  =     HEN-A18    *    HEN-A20.
*店舗名（備考）
*****MOVE        HEN-A13   TO        HEN-F22.
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    HEN-A01       TO    TOK-F01
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
     MOVE        HEN-A15   TO        HEN-F25.
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
*変換値
*****MOVE        WK-NEN    TO        HEN-F29.
*店舗名カナ
     MOVE        HEN-A13   TO        HEN-F30.
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*振分倉庫コード
     MOVE        TOK-F81   TO        HEN-F48.
*    ルート条件マスタ検索
     MOVE     ZERO         TO        INV-RUT.
     MOVE     SPACE        TO        RUT-REC.
     INITIALIZE                      RUT-REC.
     MOVE     HEN-F01      TO        RUT-F01.
     MOVE     SPACE        TO        RUT-F02.
     MOVE     HEN-A14      TO        RUT-F03  HEN-F42.
     READ     JHMRUTL1
         INVALID
           MOVE  1         TO        INV-RUT
           MOVE  TOK-F81   TO        HEN-F48
         NOT INVALID
           MOVE  RUT-F05   TO        HEN-F48
     END-READ.
*_番
     MOVE        TBL-F08   TO        HEN-F49.
*訂正前数量
     MOVE        HEN-A18   TO        HEN-F50.
*修正原価単価
     MOVE        HEN-A19   TO        HEN-F512.
*修正売価単価
     MOVE        HEN-A20   TO        HEN-F513.
*修正原価金額
     COMPUTE     HEN-F521  =     HEN-A18    *    HEN-A19.
*修正売価金額
     COMPUTE     HEN-F522  =     HEN-A18    *    HEN-A20.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JHMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JHMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    HEN-A01       TO        KEN-F03.
     IF      INV-RUT  =  ZERO
             MOVE RUT-F05  TO        KEN-F04
     ELSE
             MOVE TOK-F81  TO        KEN-F04
     END-IF.
     READ    JHMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JHMKENL1-010
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    HEN-A01       TO        KEN-F03.
     IF      INV-RUT  =  ZERO
             MOVE RUT-F05  TO        KEN-F04
     ELSE
             MOVE TOK-F81  TO        KEN-F04
     END-IF.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO    JHMKENL1-WRT-EXIT.
*
 JHMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     ADD     CNT-KENSU-D   TO        KEN-F10.
     ADD     1             TO        KEN-F11.
     REWRITE KEN-REC.
*
 JHMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        CNT-MAISU     >    ZERO
*         出荷場所件数マスタ出力
          IF   WK-DEN3-03    NOT =     ZERO
               PERFORM  JHMKENL1-WRT-SEC
          END-IF
*         当日スケジュールマスタ出力
          PERFORM  JHMTJSL1-WRT-SEC
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
               JHMKENL1  JHMTJSL1.
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
 JHMTJSL1-WRT-SEC        SECTION.
*
     MOVE   "JHMTJSL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    HEN-A01       TO        TJS-F03.
     READ    JHMTJSL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JHMTJSL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    HEN-A01       TO        TJS-F03.
     MOVE    CNT-KENSU     TO        TJS-F08.
     MOVE    CNT-MAISU     TO        TJS-F09.
     WRITE   TJS-REC.
     GO      TO    JHMTJSL1-WRT-EXIT.
*
 JHMTJSL1-010.
*
     MOVE    CNT-KENSU     TO        TJS-F08.
     MOVE    CNT-MAISU     TO        TJS-F09.
     REWRITE TJS-REC.
*
 JHMTJSL1-WRT-EXIT.
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
     MOVE      HEN-A01            TO    VLD-F13.
***  DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
***  DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
***  DISPLAY "WK-TOKCD   = " WK-TOKCD   UPON CONS.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
