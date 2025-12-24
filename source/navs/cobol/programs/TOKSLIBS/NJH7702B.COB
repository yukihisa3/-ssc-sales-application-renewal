****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ作成　　　　　　　*
*    作成日／更新日　　　：　10/09/07                          *
*    作成者／更新者　　　：　NAV ABE                           *
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*　　　　　　　　　　　　　　サンデー　　　　　　　　　　　　　*
*    作成日／更新日　　　：　15/05/13                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　店舗ＣＤ５桁へ変更　　　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH7702B.
 AUTHOR.                NAV ABE
 DATE-WRITTEN.          10/09/07.
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
*    受信データ　ＲＬ＝　１２８　  ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS  2.
         05  DEN-01A             PIC  X(01).
         05  DEN-01B             PIC  X(02).
         05  DEN-01C             PIC  X(125).
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
     COPY     SDSHIRED  OF        XFDLIB
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
 01  CNT-DENNO               PIC  9(09)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(05)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
*ルート変換
 01  HEN-RUT.
     03  HEN-RUT-1           PIC  X(01)     VALUE  "0".
     03  HEN-RUT-2           PIC  X(01)     VALUE  SPACE.
*
*ファイルヘッドレコード退避ワーク
 01  WK-DEPA-REC.
     03  WK-DEPA01          PIC  X(01).
     03  WK-DEPA02          PIC  9(02).
     03  WK-DEPA03          PIC  9(05).
     03  WK-DEPA04          PIC  9(06).
     03  WK-DEPA05          PIC  9(07).
     03  WK-DEPA06          PIC  9(01).
     03  WK-DEPA07          PIC  X(06).
     03  WK-DEPA08          PIC  X(06).
     03  WK-DEPA09          PIC  9(06).
     03  WK-DEPA10          PIC  9(05).
     03  WK-DEPA11          PIC  9(05).
     03  WK-DEPA12          PIC  X(02).
     03  WK-DEPA13          PIC  X(20).
     03  WK-DEPA14          PIC  X(06).
     03  WK-DEPA15          PIC  X(50).
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(02).
     03  WK-DEPB03          PIC  9(05).
     03  WK-DEPB04          PIC  9(06).
     03  WK-DEPB05          PIC  9(07).
     03  WK-DEPB06          PIC  9(01).
     03  WK-DEPB07          PIC  9(01).
     03  WK-DEPB08          PIC  9(03).
     03  WK-DEPB09          PIC  X(02).
     03  WK-DEPB10          PIC  X(02).
     03  WK-DEPB11          PIC  9(06).
     03  WK-DEPB12          PIC  9(01).
     03  WK-DEPB13          PIC  X(03).
     03  WK-DEPB14          PIC  X(02).
     03  WK-DEPB15          PIC  X(15).
     03  WK-DEPB16          PIC  X(16).
     03  WK-DEPB17          PIC  X(20).
     03  WK-DEPB18          PIC  X(01).
     03  WK-DEPB19          PIC  X(01).
     03  WK-DEPB20          PIC  X(01).
     03  WK-DEPB21          PIC  X(16).
     03  WK-DEPB221         PIC  9(06).
     03  WK-DEPB222.
*********2015/05/13 NAV ST 店舗ＣＤ５桁化変更
*        05  WK-DEPB222A    PIC  X(03).
*        05  WK-DEPB222B    PIC  9(04).
         05  WK-DEPB222A    PIC  X(02).
         05  WK-DEPB222B    PIC  9(05).
*********2015/05/13 NAV ED
     03  WK-DEPB223         PIC  X(03).
*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  9(02).
     03  WK-DEPD03          PIC  9(05).
     03  WK-DEPD04          PIC  9(06).
     03  WK-DEPD05          PIC  9(07).
     03  WK-DEPD06          PIC  9(01).
     03  WK-DEPD07          PIC  X(13).
**   03  WK-DEPD08          PIC  X(05).
     03  WK-DEPD09          PIC  9(05).
     03  WK-DEPD10          PIC  9(06)V9(01).
     03  WK-DEPD11          PIC  9(07).
     03  WK-DEPD12          PIC  X(24).
     03  WK-DEPD13          PIC  X(09).
     03  WK-DEPD14          PIC  X(07).
     03  WK-DEPD15          PIC  9(04).
     03  WK-DEPD16          PIC  X(06).
     03  WK-DEPD17          PIC  X(24).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  HDJ-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
*取引先ＣＤ変換（文字⇒数値）
 01  WK-TORICD             PIC  X(05).
 01  WK-TORICD-R           REDEFINES  WK-TORICD.
     03  WK-HEN-TORICD     PIC  9(05).
*
 01  WK-DENNO-H            PIC  X(07).
 01  WK-DENNO-H-R          REDEFINES  WK-DENNO-H.
     03  WK-HEN-DENNO-H    PIC  9(07).
*数量
 01  WK-SURYO              PIC S9(08).
 01  WK-SURYO-R            REDEFINES  WK-SURYO.
     03  WK-HEN-SURYO      PIC S9(06)V9(02).
*原価
 01  WK-GENKA              PIC S9(10).
 01  WK-GENKA-R            REDEFINES  WK-GENKA.
     03  WK-HEN-GENKA      PIC S9(08)V9(02).
*金額
 01  WK-YEN                PIC S9(11).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH7701B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH7701B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH7701B".
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
 INIT-010.
     OPEN     EXTEND    JHSHENL1.
 INIT-020.
     OPEN     I-O       JSMKENL1  JSMDAYL1.
 INIT-030.
     OPEN     OUTPUT    VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
 INIT-040.
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
         MOVE    ZERO             TO   SYS-DATEW
     END-IF.
 INIT-050.
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
     MOVE    "MAIN-SEC"          TO    S-NAME.
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >        2
*
         IF    DEN-01B(IDX)    =   "02"
               IF    DEN-01A(IDX)    =    "A"
                     MOVE      SPACE       TO    WK-DEPA-REC
                     INITIALIZE                  WK-DEPA-REC
                     MOVE      DEN-01(IDX) TO    WK-DEPA-REC
               END-IF
*伝票ヘッダ処理
               IF    DEN-01A(IDX)    =    "B"
*          出荷場所件数マスタ出力
                     IF    CNT-MAISU    >    ZERO
                           PERFORM  JSMKENL1-WRT-SEC
                     END-IF
                     MOVE      SPACE       TO    WK-DEPB-REC
                     INITIALIZE                  WK-DEPB-REC
                     MOVE      DEN-01(IDX) TO    WK-DEPB-REC
                     ADD       1           TO    CNT-MAISU
                     MOVE      ZERO        TO    CNT-KENSU-D
               END-IF
*明細行
               IF   DEN-01A(IDX)    =    "D"
                     MOVE      DEN-01(IDX) TO    WK-DEPD-REC
                     ADD       1           TO    CNT-KENSU
                     ADD       1           TO    CNT-KENSU-D
*          変換データ 出力
                     PERFORM  EDIT-SEC
               END-IF
         END-IF
     END-PERFORM.
*
     READ   CVCSG001
         AT END
            MOVE     9           TO    END-FG
         NOT AT END
            ADD      1           TO    RD-CNT
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
*伝票ヘッダー転送
*    レコード区分
     MOVE     WK-DEPB01     TO        HEN-A01.
*    データ区分
     MOVE     WK-DEPB02     TO        HEN-A02.
*    取引先ＣＤ
     MOVE     WK-DEPB03     TO        HEN-A03.
*    発注日
     MOVE     WK-DEPB04     TO        HEN-A04.
*    伝票番号
     MOVE     WK-DEPB05     TO        HEN-A05.
*    行番号
     MOVE     WK-DEPB06     TO        HEN-A06.
*    企業ＣＤ
     MOVE     WK-DEPB07     TO        HEN-A07.
*    店ＣＤ
     MOVE     WK-DEPB08     TO        HEN-A08.
*    分類ＣＤ
     MOVE     WK-DEPB09     TO        HEN-A09.
*    伝票区分
     MOVE     WK-DEPB10     TO        HEN-A10.
*    納品指定日
     MOVE     WK-DEPB11     TO        HEN-A11.
*    午前午後区分
     MOVE     WK-DEPB12     TO        HEN-A12.
*    メーカーＣＤ
     MOVE     WK-DEPB13     TO        HEN-A13.
*    倉庫区分
     MOVE     WK-DEPB14     TO        HEN-A14.
*    社名
     MOVE     WK-DEPB15     TO        HEN-A15.
*    店名
     MOVE     WK-DEPB16     TO        HEN-A16.
*    直送先メーカー名
     MOVE     WK-DEPB17     TO        HEN-A17.
*    Ｄ欄（１、２、３行目）
     MOVE     WK-DEPB18     TO        HEN-A18.
     MOVE     WK-DEPB19     TO        HEN-A19.
     MOVE     WK-DEPB20     TO        HEN-A20.
*    Ｇ欄用メッセージ
     MOVE     WK-DEPB21     TO        HEN-A21.
*    新取引先コード
     MOVE     WK-DEPB221    TO        HEN-A221.
*    新店舗コード
     MOVE     WK-DEPB222    TO        HEN-A222.
*    新部門コード
     MOVE     WK-DEPB223    TO        HEN-A223.
*伝票明細転送
*    レコード区分
     MOVE     WK-DEPD01     TO        HEN-A23.
*    データ区分
     MOVE     WK-DEPD02     TO        HEN-A24.
*    取引先ＣＤ
     MOVE     WK-DEPD03     TO        HEN-A25.
*    発注日
     MOVE     WK-DEPD04     TO        HEN-A26.
*    伝票番号
     MOVE     WK-DEPD05     TO        HEN-A27.
*    行番号
     MOVE     WK-DEPD06     TO        HEN-A28.
*    商品ＣＤ
     MOVE     WK-DEPD07     TO        HEN-A29.
*    予備
*    MOVE     WK-DEPD08     TO        HEN-A30.
*    発注数量
     MOVE     WK-DEPD09     TO        HEN-A31.
*    原価単価
     MOVE     WK-DEPD10     TO        HEN-A32.
*    売価金額
     MOVE     WK-DEPD11     TO        HEN-A33.
*    商品名
     MOVE     WK-DEPD12     TO        HEN-A34.
*    規格名
     MOVE     WK-DEPD13     TO        HEN-A35.
*    色
     MOVE     WK-DEPD14     TO        HEN-A36.
*    ケース入数
     MOVE     WK-DEPD15     TO        HEN-A37.
*    ピッキング_
     MOVE     WK-DEPD16     TO        HEN-A38.
*    空白
     MOVE     WK-DEPD17     TO        HEN-A39.
*    取引先名称
     MOVE     WK-DEPA13     TO        HEN-A40.
*****DISPLAY  " WK-DEPA01 = " WK-DEPA01 UPON CONS.
*****DISPLAY  " WK-DEPA02 = " WK-DEPA02 UPON CONS.
*****DISPLAY  " WK-DEPA03 = " WK-DEPA03 UPON CONS.
*****DISPLAY  " WK-DEPA04 = " WK-DEPA04 UPON CONS.
*****DISPLAY  " WK-DEPA05 = " WK-DEPA05 UPON CONS.
*****DISPLAY  " WK-DEPA06 = " WK-DEPA06 UPON CONS.
*****DISPLAY  " WK-DEPA07 = " WK-DEPA07 UPON CONS.
*****DISPLAY  " WK-DEPA08 = " WK-DEPA08 UPON CONS.
*****DISPLAY  " WK-DEPA09 = " WK-DEPA09 UPON CONS.
*****DISPLAY  " WK-DEPA10 = " WK-DEPA10 UPON CONS.
*****DISPLAY  " WK-DEPA11 = " WK-DEPA11 UPON CONS.
*****DISPLAY  " WK-DEPA12 = " WK-DEPA12 UPON CONS.
*****DISPLAY  " WK-DEPA13 = " WK-DEPA13 UPON CONS.
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
     MOVE    "TENSO-SEC"         TO        S-NAME.
*取引先コード
     MOVE    WK-DEPB221          TO        HEN-F01.
     MOVE    WK-DEPB221          TO        WK-TOKCD.
*伝票ナンバー
     MOVE    HEN-A05             TO        HEN-F02
                                           HEN-F23.
*行番号
     MOVE    HEN-A28             TO        HEN-F03.
*伝区
     MOVE        40              TO        HEN-F051.
     MOVE     NC"売上伝票"       TO        HEN-F052.
*担当者コード
     MOVE        99              TO        HEN-F06.
*店コード
     MOVE    WK-DEPB222B         TO        HEN-F07.
*  商品変換テーブル検索（ＪＡＮＣＤで検索）
     MOVE        SPACE           TO        TBL-REC.
     INITIALIZE                            TBL-REC.
     MOVE        HEN-F01         TO        TBL-F01.
     MOVE        HEN-A29         TO        TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE           TO        TBL-REC
         INITIALIZE                        TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04         TO        HEN-F08
                                           HEN-F09.
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE       "3"              TO        LINK-IN-KBN.
     MOVE        HEN-A04         TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"       USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8   TO   HEN-F111
     ELSE
         MOVE    ZERO            TO   HEN-F111
     END-IF.
*納品日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE       "3"              TO        LINK-IN-KBN.
     MOVE        HEN-A11         TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"       USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8.
     IF          LINK-OUT-RET    =     ZERO
         MOVE    LINK-OUT-YMD8   TO        HEN-F112
     ELSE
         MOVE    ZERO            TO        HEN-F112
     END-IF.
*****MOVE        HEN-F112        TO        HEN-F113.
*分類（部門）
*****MOVE        HEN-A09         TO        HEN-F12.
     MOVE        HEN-A223        TO        HEN-F12.
*商品区分　　
     MOVE        SPACE           TO        HEN-F131.
*伝票区分　　
     MOVE        HEN-A10         TO        HEN-F132.
*伝発区分
     MOVE        9               TO        HEN-F134.
*自社商品コード
     MOVE        TBL-F031        TO        HEN-F1411.
*自社商品単品コード
     MOVE        TBL-F032        TO        HEN-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE        SPACE           TO        MEI-REC.
     INITIALIZE                            MEI-REC.
     MOVE        TBL-F031        TO        MEI-F011.
     MOVE        TBL-F032        TO        MEI-F012.
     READ    MEIMS1
       INVALID
*        商品名
         MOVE    HEN-A34(1:15)   TO   HEN-F1421
         MOVE    HEN-A34(16:9)   TO   HEN-F1422(1:9)
         MOVE    HEN-A35(1:6)    TO   HEN-F1422(1:6)
       NOT INVALID
         MOVE    HEN-A34(1:15)   TO   HEN-F1421
         MOVE    HEN-A34(16:9)   TO   HEN-F1422(1:9)
         MOVE    HEN-A35(1:6)    TO   HEN-F1422(1:6)
     END-READ.
*数量
     MOVE        HEN-A31         TO        HEN-F15.
*単
     MOVE       "1"              TO        HEN-F16.
*原価単価
*****DISPLAY "TBL-F05 = " TBL-F05  UPON CONS.
     MOVE        HEN-A32         TO        HEN-F172.
*売価単価
     MOVE        HEN-A33         TO        HEN-F173.
*原価金額
     COMPUTE     HEN-F181  =  HEN-A31  *  HEN-A32.
*売価金額
     COMPUTE     HEN-F182  =  HEN-A31  *  HEN-A33.
     MOVE        HEN-A32         TO        HEN-F182.
*店舗名（備考）
     MOVE        HEN-A16         TO        HEN-F22.
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE           TO        TOK-REC
         INITIALIZE                        TOK-REC
         MOVE    HEN-F01         TO        TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE       TO        TOK-REC
               INITIALIZE                  TOK-REC
         END-READ
         MOVE    TOK-F52         TO        HEN-F24
         MOVE    1               TO        FLG-TOK
     ELSE
         MOVE    TOK-F52         TO        HEN-F24
     END-IF.
*相手商品コード
     MOVE        HEN-A29         TO        HEN-F25.
*伝票発行区分
     MOVE        9               TO        HEN-F272.
*オンライン区分
     MOVE        1               TO        HEN-F274.
*エントリー区分
     MOVE        1               TO        HEN-F275.
*付番区分
     MOVE        9               TO        HEN-F276.
*量販店区分
     MOVE       "A"              TO        HEN-F278.
*ＷＳ_
     MOVE        1               TO        HEN-F28.
*変換値
*****MOVE        WK-NEN          TO        HEN-F29.
*店舗名（カナ）
     MOVE        HEN-A10         TO        HEN-F30.
*システム日付
     MOVE        SYS-DATEW       TO        HEN-F99.
*ルート（直送区分）
     MOVE        HEN-A13         TO        HEN-F42.
*受信日付
     MOVE     PARA-JDATE         TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME         TO        HEN-F47.
*振分倉庫コード
*ルート条件マスタ検索
     MOVE     ZERO               TO        INV-RUT.
     MOVE     SPACE              TO        RUT-REC.
     INITIALIZE                            RUT-REC.
*取引先ＣＤは取引先エリアの取引先ＣＤを使用すること
     MOVE     HEN-F01            TO        RUT-F01.
     MOVE     SPACE              TO        RUT-F02.
     MOVE     HEN-A13            TO        RUT-F03.
*****MOVE     "00"               TO        RUT-F03.
     READ     JHMRUTL1
         INVALID
           MOVE  1               TO        INV-RUT
           MOVE  TOK-F81         TO        HEN-F48
         NOT INVALID
           MOVE  RUT-F05         TO        HEN-F48
     END-READ.
*_番
     MOVE        TBL-F08         TO        HEN-F49.
*訂正前数量
     MOVE        HEN-A31         TO        HEN-F50.
*修正原価単価
     MOVE        HEN-A32         TO        HEN-F512.
*修正売価単価
     MOVE        HEN-A33         TO        HEN-F513.
*修正原価金額
     MOVE        HEN-F181        TO        HEN-F521.
*修正売価金額
     MOVE        HEN-F182        TO        HEN-F522.
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
     MOVE    WK-TOKCD      TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
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
     MOVE    WK-TOKCD      TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
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
               PERFORM   JSMKENL1-WRT-SEC
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
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD      TO        TJS-F03.
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
     MOVE    WK-TOKCD      TO        TJS-F03.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     WRITE   TJS-REC.
     GO      TO    JSMDAYL1-WRT-EXIT.
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
     MOVE      WK-TOKCD           TO    VLD-F13.
     DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
     DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
     DISPLAY "WK-TOKCD   = " WK-TOKCD   UPON CONS.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
