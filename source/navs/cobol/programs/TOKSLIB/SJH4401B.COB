****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成　　　　　　　　*
*    作成日／更新日　　　：　02/09/25                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*                            バローフジヤマ　　　　　　　　　　*
*　　ルート取得方法：明細一行目の変換テーブル登録の出荷場所を　*
*　　　　　　　　　　ルートとして使用する。　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH4401B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/09/25.
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
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS 2.
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
     COPY     BRSHIRED  OF        XFDLIB
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
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(06)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
*
*ファイルヘッダー退避ワーク
 01  WK-DEPA-REC.
     03  WK-DEPA01          PIC  X(02).
     03  WK-DEPA02          PIC  X.
     03  WK-DEPA03          PIC  9(08).
     03  WK-DEPA04          PIC  9(06).
     03  WK-DEPA05          PIC  X(04).
     03  WK-DEPA06          PIC  X(20).
     03  WK-DEPA07          PIC  9(07).
     03  WK-DEPA08          PIC  X(20).
     03  WK-DEPA09          PIC  X(60).
*
*伝票ヘッダー退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X.
     03  WK-DEPB02          PIC  9(02).
     03  WK-DEPB03          PIC  9(06).
     03  WK-DEPB04          PIC  9(02).
     03  WK-DEPB05          PIC  9(02).
     03  WK-DEPB06          PIC  9(05).
     03  WK-DEPB07          PIC  X(25).
     03  WK-DEPB08          PIC  9(05).
     03  WK-DEPB09          PIC  X(11).
     03  WK-DEPB10          PIC  X(12).
     03  WK-DEPB11          PIC  X(01).
     03  WK-DEPB12          PIC  X(12).
     03  WK-DEPB13          PIC  X(12).
     03  WK-DEPB14          PIC  9(06).
     03  WK-DEPB15          PIC  9(06).
     03  WK-DEPB16          PIC  X(20).
*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X.
     03  WK-DEPD02          PIC  9.
     03  WK-DEPD03          PIC  9(08).
     03  WK-DEPD04          PIC  X(30).
     03  WK-DEPD05          PIC  9(07).
     03  WK-DEPD06          PIC  9(09).
     03  WK-DEPD07          PIC  9(07).
     03  WK-DEPD08          PIC  X(13).
     03  WK-DEPD09          PIC  X(08).
     03  WK-DEPD10          PIC  X(44).
*
 01  WRK-SNAME.
     03  WRK-SNAME1        PIC  X(20).
     03  WRK-SNAME2        PIC  X(20).
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC  9(06).
     03  SYS-DATEW         PIC  9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH4401B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH4401B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH4401B".
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
                        PROCEDURE   JHMKENL1.
     MOVE      "JHMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
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
 FILEERR-SEC7           SECTION.
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
                        SHOTBL1
                        JHMRUTL1  TOKMS2.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JHMKENL1  JHMTJSL1.
     OPEN     OUTPUT    VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DEPA-REC.
     INITIALIZE                   WK-DEPA-REC.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
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
*ファイルヘッダの場合は次レコード読込み
     IF    DEN-01A(1)  =     "H"
           GO                    TO   MAIN010
     END-IF.
*
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >         2
*伝票ヘッダ処理
        IF    DEN-01A(IDX)  =     "B"
*             出荷場所件数マスタ出力
              IF   CNT-MAISU     >    ZERO
                   PERFORM  JHMKENL1-WRT-SEC
              END-IF
              MOVE      SPACE         TO    WK-DEPB-REC
              INITIALIZE                    WK-DEPB-REC
              MOVE      DEN-01(IDX)   TO    WK-DEPB-REC
              ADD       1        TO   CNT-MAISU
              MOVE      ZERO     TO   CNT-KENSU-D
        END-IF
*
*明細レコード
        IF    DEN-01A(IDX)  =     "D"
              MOVE      DEN-01(IDX)   TO    WK-DEPD-REC
              ADD       1        TO   CNT-KENSU
              ADD       1        TO   CNT-KENSU-D
        END-IF
        IF    DEN-01A(IDX)  =     "D"
              PERFORM                       EDIT-SEC
        END-IF
*
     END-PERFORM.
*
 MAIN010.
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
*    ヘッダレコード区分
     MOVE     WK-DEPB01     TO        HEN-A01.
*    店ＣＤ
     MOVE     WK-DEPB02     TO        HEN-A02.
*    伝票番号
     MOVE     WK-DEPB03     TO        HEN-A03.
*    伝票区分
     MOVE     WK-DEPB04     TO        HEN-A04.
*    分類ＣＤ
     MOVE     WK-DEPB05     TO        HEN-A05.
*    仕入先ＣＤ
     MOVE     WK-DEPB06     TO        HEN-A06.
*    仕入先名カナ
     MOVE     WK-DEPB07     TO        HEN-A07.
*    直送先ＣＤ
     MOVE     WK-DEPB08     TO        HEN-A08.
*    店名
     MOVE     WK-DEPB09     TO        HEN-A09.
*    会社名
     MOVE     WK-DEPB10     TO        HEN-A10.
*    本部入力区分
     MOVE     WK-DEPB11     TO        HEN-A11.
*    摘要１
     MOVE     WK-DEPB12     TO        HEN-A12.
*    摘要２
     MOVE     WK-DEPB13     TO        HEN-A13.
*    発注日
     MOVE     WK-DEPB14     TO        HEN-A14.
*    納品日
     MOVE     WK-DEPB15     TO        HEN-A15.
*    予備
     MOVE     WK-DEPB16     TO        HEN-A16.
*
*    レコード区分
     MOVE     WK-DEPD01     TO        HEN-A17.
*    伝票行番号
     MOVE     WK-DEPD02     TO        HEN-A18.
*    商品ＣＤ
     MOVE     WK-DEPD03     TO        HEN-A19.
*    商品名
     MOVE     WK-DEPD04     TO        HEN-A20.
*    数量
     MOVE     WK-DEPD05     TO        HEN-A21.
*    原価単価
     MOVE     WK-DEPD06     TO        HEN-A22.
*    売価単価
     MOVE     WK-DEPD07     TO        HEN-A23.
*    ＪＡＮＣＤ
     MOVE     WK-DEPD08     TO        HEN-A24.
*    取引先商品ＣＤ
     MOVE     WK-DEPD09     TO        HEN-A25.
*    予備
*****
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
     MOVE        HEN-A06   TO        HEN-F01.
     MOVE        HEN-A06   TO        WK-TOKCD.
*店コード
     MOVE        HEN-A02   TO        HEN-F07.
*伝票ナンバー（店舗ＣＤ＋伝票番号をサカタ管理伝票番号とする）
     COMPUTE     HEN-F02  =  HEN-F07 * 10000000 + HEN-A03.
*****MOVE        HEN-A03   TO        HEN-F02.
     MOVE        HEN-F02   TO        HEN-F23.
*行_
     MOVE        HEN-A18   TO        HEN-F03.
*伝区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*  商品変換テーブル検索
     MOVE        SPACE     TO        TBL-REC.
     INITIALIZE                      TBL-REC.
     MOVE        HEN-F01   TO        TBL-F01.
     MOVE    HEN-A191      TO        TBL-F02(1:2).
     MOVE    HEN-A192      TO        TBL-F02(3:5).
     MOVE    HEN-A193      TO        TBL-F02(8:1).
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*振分ルート取得
     IF  HEN-F02  NOT =  WK-DENNO
         MOVE   TBL-F04    TO        WK-RUTO
         MOVE   HEN-F02    TO        WK-DENNO
     END-IF.
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A14   TO        LINK-IN-YMD6.
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
     MOVE        HEN-A15   TO        LINK-IN-YMD6.
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
*出荷日はセットしない
*
*分類（部門）
     MOVE        HEN-A05   TO        HEN-F12.
*商品区分　　
     MOVE        SPACE     TO        HEN-F131.
*伝票区分　　
     MOVE        HEN-A04   TO        HEN-F132.
*伝発区分
     MOVE        9         TO        HEN-F134.
*自社商品コード
     MOVE        TBL-F031  TO        HEN-F1411.
*自社商品単品コード
     MOVE        TBL-F032  TO        HEN-F1412.
*商品名　　　
     MOVE        HEN-A20(1:15)  TO   HEN-F1421.
     MOVE        HEN-A20(16:15) TO   HEN-F1422.
*数量
     MOVE        HEN-A21   TO        HEN-F15.
*単
     MOVE       "1"        TO        HEN-F16.
*原価単価
     COMPUTE  HEN-F172  =  HEN-A22  /  100.
*売価単価
     MOVE        HEN-A23   TO        HEN-F173.
*原価金額
     COMPUTE  HEN-F181  =  HEN-F15  *  HEN-F172.
*売価金額
     COMPUTE  HEN-F182  =  HEN-F15  *  HEN-F173.
*店舗名（備考）
*****MOVE        HEN-AXX   TO        HEN-F22.
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    HEN-F01       TO    TOK-F01
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
     MOVE        HEN-A191  TO        HEN-F25(1:2).
     MOVE        HEN-A192  TO        HEN-F25(3:5).
     MOVE        HEN-A193  TO        HEN-F25(8:1).
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
*
*店舗名（カナ）
     MOVE        HEN-A09   TO        HEN-F30.
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*ルート（納品時間）
*****
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*振分倉庫コード
*?   ルート条件マスタ検索
*?   MOVE     ZERO         TO        INV-RUT.
*?   MOVE     SPACE        TO        RUT-REC.
*?   INITIALIZE                      RUT-REC.
*取引先ＣＤは取引先エリアの取引先ＣＤを使用すること
*?   MOVE     HEN-A07      TO        RUT-F01.
*?   MOVE     SPACE        TO        RUT-F02.
*?   MOVE     HEN-A28      TO        RUT-F03.
*?   READ     JHMRUTL1
*?       INVALID
*?         MOVE  1         TO        INV-RUT
*?         MOVE  TOK-F81   TO        HEN-F48
*?       NOT INVALID
*?         MOVE  RUT-F05   TO        HEN-F48
*?   END-READ.
*ルートＣＤのセット 2002/03/06 追加
*ルートへ振分先ＣＤセット
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        HEN-F48  HEN-F42
     ELSE
           MOVE  WK-RUTO   TO        HEN-F48  HEN-F42
     END-IF.
*_番
     MOVE        TBL-F08   TO        HEN-F49.
*訂正前数量
     MOVE        HEN-F15   TO        HEN-F50.
*修正原価単価
     MOVE        HEN-F172  TO        HEN-F512.
*修正売価単価
     MOVE        HEN-F173  TO        HEN-F513.
*修正原価金額
     MOVE        HEN-F181  TO        HEN-F521.
*修正売価金額
     MOVE        HEN-F182  TO        HEN-F522.
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
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD      TO        KEN-F03.
*****IF      INV-RUT       =         ZERO
*****        MOVE  RUT-F05     TO    KEN-F04
*****ELSE
*****        MOVE  TOK-F81     TO    KEN-F04
*****END-IF.
*ルートＣＤのセット
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        KEN-F04
     ELSE
           MOVE  WK-RUTO   TO        KEN-F04
     END-IF.
*****
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
     MOVE    WK-TOKCD      TO        KEN-F03.
*****IF      INV-RUT       =         ZERO
*****        MOVE  RUT-F05     TO    KEN-F04
*****ELSE
*****        MOVE  TOK-F81     TO    KEN-F04
*****END-IF.
*ルートＣＤのセット
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        KEN-F04
     ELSE
           MOVE  WK-RUTO   TO        KEN-F04
     END-IF.
*****
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JHMKENL1-WRT-EXIT.
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
*              出荷場所件数マスタ出力
               PERFORM   JHMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
               PERFORM   JHMTJSL1-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     CVCSG001  JHSHENL1
               SHOTBL1
               JHMRUTL1  TOKMS2
               JHMKENL1  JHMTJSL1.
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
               DISPLAY "AAA" UPON CONS
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
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD      TO        TJS-F03.
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
     MOVE    WK-TOKCD      TO        TJS-F03.
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
     MOVE      WK-TOKCD           TO    VLD-F13.
     DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
     DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
     DISPLAY "WK-TOKCD   = " WK-TOKCD   UPON CONS.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
