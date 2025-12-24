# STE1601B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STE1601B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　オンライン　　　　　　　　　　　　
*    モジュール名　　　　：　手書伝票発行（島忠）　　　　　　　
*    作成日／更新日　　　：　93/08/04
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　
*    処理概要　　　　　　：　手書伝票の発行を行う　　　　　　　
*    更新履歴            ：
*     2011/10/05 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE1601B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          93/09/06.
******************************************************************
 ENVIRONMENT            DIVISION.
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票データ
     SELECT   HDENJNL        ASSIGN    TO        DA-01-VI-JHTTEGL1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   SEQUENTIAL
                             RECORD    KEY  IS   DEN-F01
                                                 DEN-F02
                                                 DEN-F04
                                                 DEN-F051
                                       *> 2011/10/05,S  S.I/NAV
                                                 DEN-F07
                                                 DEN-F112
                                       *> 2011/10/05,E  S.I/NAV
                                                 DEN-F03
                             FILE STATUS    IS   DEN-ST.
*店舗マスタ
**** SELECT   HTENMS         ASSIGN    TO        DA-01-VI-TENMS1
****                         ORGANIZATION   IS   INDEXED
****                         ACCESS    MODE IS   RANDOM
*****                        RECORD    KEY  IS   TEN-F52
****                                             TEN-F011
****                         FILE STATUS    IS   TEN-ST.
*条件ファイル
     SELECT   HJYOKEN        ASSIGN    TO        DA-01-VI-JYOKEN1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   DYNAMIC
                             RECORD    KEY  IS   JYO-F01
                                                 JYO-F02
                             FILE STATUS    IS   JYO-ST.
*プリンター
     SELECT     PRTF         ASSIGN    TO        GS-PRTF
                             DESTINATION        "PRT"
                             FORMAT              PRT-FORM
                             GROUP               PRT-GRP
                             PROCESSING          PRT-PROC
                             UNIT CONTROL        PRT-CTL
                             FILE      STATUS    PRT-ST.
*画面ファイル
     SELECT   DSPF           ASSIGN    TO        GS-DSPF
                             FORMAT              DSP-FMT
                             GROUP               DSP-GRP
                             PROCESSING          DSP-PRO
                             FUNCTION            DSP-FNC
                             STATUS              DSP-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*伝票データ
 FD  HDENJNL
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
     JOINING     DEN       AS        PREFIX.
*店舗マスタ
*FD  HTENMS
**** BLOCK       CONTAINS   8        RECORDS
**** LABEL       RECORD    IS        STANDARD.
**** COPY        HTENMS    OF        XFDLIB
**** JOINING     TEN       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     BLOCK       CONTAINS  24        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE16011  OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE16012  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*** ｽﾃｰﾀｽ
 01  ST-AREA.
     03  DEN-ST                   PIC  X(02).
     03  DEN-ST1                  PIC  X(04).
     03  TEN-ST                   PIC  X(02).
     03  TEN-ST1                  PIC  X(04).
     03  JYO-ST                   PIC  X(02).
     03  JYO-ST1                  PIC  X(04).
     03  DSP-ST                   PIC  X(02).
     03  DSP-ST1                  PIC  X(04).
     03  PRT-ST                   PIC  X(02).
     03  PRT-ST1                  PIC  X(04).
     03  IN-DATA                  PIC  X(01).
*%*表示パラメータ*%*
 01  FORM-PARA.
     03  DSP-FMT                  PIC X(08).
     03  DSP-PRO                  PIC X(02).
     03  DSP-GRP                  PIC X(08).
     03  DSP-FNC                  PIC X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL            PIC X(04).
         05  DSP-STR-PG           PIC X(02).
     03  PRT-FORM                 PIC X(08).
     03  PRT-PROC                 PIC X(02).
     03  PRT-GRP                  PIC X(08).
     03  PRT-CTL.
         05  PRT-CNTRL            PIC X(04).
         05  PRT-STR-PG           PIC X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  9(01)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
*
 01  WK-DENNO                     PIC  9(09)     VALUE   ZERO.
 01  WK-CHG                       PIC  9(11).
 01  WK-CHGR                      REDEFINES      WK-CHG.
     03 WK-CHGR1                  PIC  9(01)     OCCURS  11.
 01  BD-CHG1                      PIC  X(12).
 01  BD-CHG1X                     REDEFINES      BD-CHG1.
     03 XX-CHGR1X                 PIC  X(01)     OCCURS  12.
 01  X                            PIC  9(02)     VALUE   ZERO.
 01  Y                            PIC  9(02)     VALUE   ZERO.
 01  Z                            PIC  9(02)     VALUE   ZERO.
 01  NUM-FLG                      PIC  9(01)     VALUE   ZERO.
 01  WK-DEN112                    PIC  9(08)     VALUE   ZERO.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  G-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  G-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ﾋﾞｺｳ AREA
 01  WK-BIKO.
     03  WK-BIKO1                 PIC  X(15)     VALUE   SPACE.
     03  WK-BIKO2                 PIC  X(15)     VALUE   SPACE.
*数量
 01  WK-DEN15                     PIC  9(05)V9.
 01  WK-DEN15R     REDEFINES      WK-DEN15.
     03  WK-DEN15A                PIC  ZZZZ9.
     03  WK-DEN15B                PIC  Z.
 01  WK-DEN15AX                   PIC  X(05).
*原価単価
 01  WK-DEN172                    PIC  9(07)V99.
 01  WK-DEN172R    REDEFINES      WK-DEN172.
     03  WK-DEN172A               PIC  ZZZZZZ9.
     03  WK-DEN172B               PIC  ZZ.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WK-MAI                   PIC  9(06)     VALUE   ZERO.
     03  WRK-R040                 PIC  9(01)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  I                        PIC  9(01)     VALUE   ZERO.
     03  DENPYO.
         05  FILLER               PIC  X(22)     VALUE
             "ｼｲﾚ ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳ ﾏｲｽｳ ".
         05  CNT-DENPYO           PIC  9(09)     VALUE   ZERO.
*--- ﾃﾞﾝﾋﾟﾖｳ ｷ-
     03  WRK-DENNO.
         05  WK-DEN               PIC  9(09)     VALUE   ZERO.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG01                PIC  N(30)     VALUE
         NC"対象データありません".
     03  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください".
     03  MSG03                PIC  N(30)     VALUE
         NC"『仕入伝票発行中』".
     03  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG05                PIC  N(30)     VALUE
         NC"開始が終了より大きいです".
*
 01  DEN-ERR                      PIC  N(11)     VALUE
         NC"伝票データ　異常！！".
 01  TEN-ERR                      PIC  N(11)     VALUE
         NC"店舗マスタ　異常！！".
 01  JYO-ERR                      PIC  N(11)     VALUE
         NC"条件ファイル異常！！".
 01  DSP-ERR                      PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  PRT-ERR                      PIC  N(11)     VALUE
         NC"プリンター　異常！！".
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*伝票データ
 DEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HDENJNL.
     DISPLAY  DEN-ERR          UPON    CONS.
     DISPLAY  DEN-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*店舗マスタ
*TEN-ERR                SECTION.
**** USE AFTER          EXCEPTION      PROCEDURE      HTENMS.
**** DISPLAY  TEN-ERR          UPON    CONS.
**** DISPLAY  TEN-ST           UPON    CONS.
**** ACCEPT   IN-DATA          FROM    CONS.
**** MOVE     4000             TO      PROGRAM-STATUS.
**** STOP     RUN.
*条件ファイル
 JYO-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HJYOKEN.
     DISPLAY  JYO-ERR          UPON    CONS.
     DISPLAY  JYO-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*プリンター
 PRT-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTF.
     DISPLAY  PRT-ERR          UPON    CONS.
     DISPLAY  PRT-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*画面ファイル
 DSP-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPF.
     DISPLAY  DSP-ERR          UPON    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 PROC-SEC                  SECTION.
     OPEN        I-O       DSPF.
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG = 1 OR 9.
     PERFORM     END-SEC.
     IF  (END-FLG = 1)
         MOVE    ZERO     TO         END-FLG
         GO               TO         PROC-010.
     CLOSE       DSPF.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     OPEN        INPUT     HDENJNL
                           HJYOKEN
                 OUTPUT    PRTF.
*
     MOVE     ZERO               TO   WK-DEN  WK-MAI WK-DENNO.
     MOVE     ZERO               TO   CNT-DENPYO.
*伝票総枚数読込み
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE            TO  DEN-REC.
     INITIALIZE  DEN-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     72028              TO   DEN-F01.
     MOVE     ZERO               TO   DEN-F02.
     MOVE     ZERO               TO   DEN-F04.
     MOVE     40                 TO   DEN-F051.
     MOVE     ZERO               TO   DEN-F03.
*  2011/10/05,S  S.I/NAV
**     START    HDENJNL   KEY  IS  >=   DEN-F01  DEN-F02  DEN-F04
**                                      DEN-F051 DEN-F03
**              INVALID
**              DISPLAY NC"出力対象無し" UPON CONS
**              STOP  RUN
**     END-START.
     START    HDENJNL   KEY  IS  >=   DEN-F01  DEN-F02  DEN-F04
                                      DEN-F051 DEN-F07  DEN-F112
                                      DEN-F03
              INVALID
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-START.
*  2011/10/05,E  S.I/NAV
 INIT001.
*伝票総枚数読込み
     READ     HDENJNL  AT  END
         IF   WK-MAI  =  ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
         ELSE
              GO                 TO   INIT002
         END-IF
     END-READ.
*取引先がブレイクの場合、読み飛ばし
     IF       DEN-F01   >  72028
         IF   WK-MAI  =  ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
         ELSE
              GO                 TO   INIT002
         END-IF
     END-IF.
*伝票行が８０以上の場合、読み飛ばし
     IF       DEN-F03  >=  80
****          IF    DEN-F03  =  80
****                MOVE   DEN-F1421  TO    WK-BIKO1
****                MOVE   DEN-F1422  TO    WK-BIKO2
****                DISPLAY "DEN-F1421 = " DEN-F1421 UPON CONS
****                DISPLAY "DEN-F1422 = " DEN-F1422 UPON CONS
****                DISPLAY "WK-BIKO1  = " WK-BIKO1  UPON CONS
****                DISPLAY "WK-BIKO2  = " WK-BIKO2  UPON CONS
****          END-IF
              GO                 TO   INIT001
     END-IF.
*伝票ブレイクチェック
     IF       DEN-F02  NOT =  WK-DENNO
              ADD      1         TO   WK-MAI
              MOVE     DEN-F02   TO   WK-DENNO
     END-IF.
*
     GO                          TO   INIT001.
*
*****MOVE     DEN-F98            TO   WK-MAI.
 INIT002.
     CLOSE    HDENJNL.
*
     OPEN     INPUT     HDENJNL.
************
*帳票初期化*
************
     MOVE        SPACE     TO        FTE16011.
     MOVE        SPACE     TO        FTE16012.
 INIT-EXIT.
     EXIT.
***********************************************************
*                       画面処理                          *
***********************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-WRT-SEC.
 MAIN-010.
     MOVE         "KUBUN"    TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE    9         TO        END-FLG
           GO                TO        MAIN-EXIT
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           GO                TO        MAIN-010
     END-EVALUATE.
*
     MOVE        SPACE       TO        ERRMSG.
     PERFORM     DSP-WRT-SEC.
     EVALUATE      KUBUN
         WHEN      "1"
                   PERFORM   TEST-PRT-SEC
         WHEN      "2"
                   MOVE      ZERO      TO   I
                   MOVE      ZERO      TO   KAIDEN
                   MOVE      ALL "9"   TO   ENDDEN
                   PERFORM   DEN-START-SEC
                   PERFORM   DEN-READ-SEC
                             UNTIL     END-FLG   =   1
         WHEN      "3"
                   MOVE      ZERO      TO   I
                   PERFORM   KEY-IN-SEC
                   PERFORM   DEN-START-SEC
                   PERFORM   DEN-READ-SEC
                             UNTIL     END-FLG   =   1
         WHEN      OTHER
                   MOVE      MSG02     TO   ERRMSG
     END-EVALUATE.
     PERFORM       DSP-WRT-SEC.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     IF      (WK-DEN  NOT =   ZERO)
              PERFORM   TAIL-EDT-SEC
              PERFORM   DENP-WRT-SEC
     END-IF.
     CLOSE                 HDENJNL  HJYOKEN   PRTF.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-EDT-SEC                 SECTION.
*
*店舗名
     MOVE        DEN-F30   TO        HD01.
*店舗ＣＤ　　　　
     MOVE        DEN-F07   TO        HD02.
*部門
     MOVE        DEN-F12   TO        HD03.
*伝票区分
     MOVE        DEN-F132  TO        HD04.
*納品日
     MOVE        SPACE     TO        HD05.
     MOVE        DEN-F112  TO        WK-DEN112.
     MOVE        WK-DEN112(3:1)  TO  HD05(1:1).
     MOVE        WK-DEN112(4:1)  TO  HD05(3:1).
     MOVE        WK-DEN112(5:1)  TO  HD05(4:1).
     MOVE        WK-DEN112(6:1)  TO  HD05(6:1).
     MOVE        WK-DEN112(7:1)  TO  HD05(7:1).
     MOVE        WK-DEN112(8:1)  TO  HD05(9:1).
*
 HEAD-EDT-90.
*
*仕入先ＣＤ
     MOVE        SPACE     TO        HD06.
     MOVE        "S"             TO  HD06(2:1).
     MOVE        "3"             TO  HD06(4:1).
     MOVE        "1"             TO  HD06(6:1).
     MOVE        DEN-F01(4:1)    TO  HD06(8:1).
     MOVE        DEN-F01(5:1)    TO  HD06(10:1).
     MOVE        DEN-F01(6:1)    TO  HD06(12:1).
     MOVE        DEN-F01(7:1)    TO  HD06(14:1).
     MOVE        DEN-F01(8:1)    TO  HD06(16:1).
     MOVE        DEN-F02         TO  W001.
*
*
 HEAD-EDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 BODY-EDT-SEC                  SECTION.
     ADD         1         TO        I.
*商品コード，商品名
     MOVE        SPACE     TO        BD23(I).
     MOVE        1         TO        Y.
     MOVE        1         TO        Z.
     PERFORM     VARYING   X   FROM  1     BY   1
                 UNTIL     X         >     7
********DISPLAY "X = " X UPON CONS
********DISPLAY "Y = " Y UPON CONS
                 MOVE      DEN-F25(X:1)    TO   BD23(I)(Y:1)
                 IF        Z         =     1
                           ADD       2     TO   Y
                           MOVE      2     TO   Z
                 ELSE
                           ADD       1     TO   Y
                           MOVE      1     TO   Z
                 END-IF
     END-PERFORM.
     MOVE        DEN-F1421 TO        BD11(I).
     MOVE        DEN-F1422 TO        BD21(I).
*数量
 BODY-EDT-10.
     MOVE        DEN-F15   TO        WK-DEN15.
     MOVE        WK-DEN15A TO        WK-DEN15AX.
     MOVE        SPACE     TO        BD24(I).
     MOVE        1         TO        Y.
     MOVE        1         TO        Z.
     MOVE        ZERO      TO        NUM-FLG.
     PERFORM     VARYING   X   FROM  1     BY   1
                 UNTIL     X         >     5
                 IF        NUM-FLG   =     ZERO
                     IF    WK-DEN15A(X:1)  NOT =     "0"
                           MOVE   1  TO    NUM-FLG
                     END-IF
                 END-IF
*
                 MOVE      WK-DEN15A(X:1)  TO   BD24(I)(Y:1)
                 IF        NUM-FLG       = ZERO
                     IF    BD24(I)(Y:1)  = ZERO
                           MOVE   SPACE  TO     BD24(I)(Y:1)
                     END-IF
                 END-IF
*
                 IF        Z         =     1
                           ADD       1     TO   Y
                           MOVE      2     TO   Z
                 ELSE
                           ADD       2     TO   Y
                           MOVE      1     TO   Z
                 END-IF
     END-PERFORM.
*原単価，売単価
 BODY-EDT-20.
     MOVE        DEN-F172  TO        WK-DEN172.
     MOVE        SPACE     TO        BD25(I).
     MOVE        1         TO        Y.
     MOVE        1         TO        Z.
     MOVE        ZERO      TO        NUM-FLG.
     PERFORM     VARYING   X   FROM  1     BY   1
                 UNTIL     X         >     7
                 IF        NUM-FLG   =     ZERO
                     IF    WK-DEN172A(X:1) NOT =     "0"
                           MOVE   1  TO    NUM-FLG
                     END-IF
                 END-IF
*
                 MOVE      WK-DEN172A(X:1) TO   BD25(I)(Y:1)
                 IF        NUM-FLG       = ZERO
                     IF    BD25(I)(Y:1)  = ZERO
                           MOVE   SPACE  TO     BD25(I)(Y:1)
                     END-IF
                 END-IF
*
                 IF        Z         =     1
                           ADD       2     TO   Y
                           MOVE      2     TO   Z
                 ELSE
                           ADD       1     TO   Y
                           MOVE      1     TO   Z
                 END-IF
     END-PERFORM.
*
 BODY-EDT-30.
     MOVE        DEN-F173  TO        WK-DEN172.
     MOVE        SPACE     TO        BD27(I).
     MOVE        1         TO        Y.
     MOVE        1         TO        Z.
     MOVE        ZERO      TO        NUM-FLG.
     PERFORM     VARYING   X   FROM  1     BY   1
                 UNTIL     X         >     7
                 IF        NUM-FLG   =     ZERO
                     IF    WK-DEN172A(X:1) NOT =     "0"
                           MOVE   1  TO    NUM-FLG
                     END-IF
                 END-IF
*
                 MOVE      WK-DEN172A(X:1) TO   BD27(I)(Y:1)
                 IF        NUM-FLG       = ZERO
                     IF    BD27(I)(Y:1)  = ZERO
                           MOVE   SPACE  TO     BD27(I)(Y:1)
                     END-IF
                 END-IF
*
                 IF        Z         =     1
                           ADD       2     TO   Y
                           MOVE      2     TO   Z
                 ELSE
                           ADD       1     TO   Y
                           MOVE      1     TO   Z
                 END-IF
     END-PERFORM.
*原価金額，売価金額
 BODY-EDT-40.
     COMPUTE     W-GENKA  =  DEN-F15  *  DEN-F172.
*
     MOVE        W-GENKA   TO        WK-DEN172.
     MOVE        SPACE     TO        BD26(I).
     MOVE        1         TO        Y.
     MOVE        1         TO        Z.
     MOVE        ZERO      TO        NUM-FLG.
     PERFORM     VARYING   X   FROM  1     BY   1
                 UNTIL     X         >     7
                 IF        NUM-FLG   =     ZERO
                     IF    WK-DEN172A(X:1) NOT =     "0"
                           MOVE   1  TO    NUM-FLG
                     END-IF
                 END-IF
*
                 MOVE      WK-DEN172A(X:1) TO   BD26(I)(Y:1)
                 IF        NUM-FLG       = ZERO
                     IF    BD26(I)(Y:1)  = ZERO
                           MOVE   SPACE  TO     BD26(I)(Y:1)
                     END-IF
                 END-IF
*
                 IF        Z         =     1
                           ADD       1     TO   Y
                           MOVE      2     TO   Z
                 ELSE
                           ADD       2     TO   Y
                           MOVE      1     TO   Z
                 END-IF
     END-PERFORM.
 BODY-EDT-50.
     COMPUTE     W-BAIKA  =  DEN-F15  *  DEN-F173.
     MOVE        W-BAIKA   TO        WK-DEN172.
     MOVE        SPACE     TO        BD28(I).
     MOVE        1         TO        Y.
     MOVE        1         TO        Z.
     MOVE        ZERO      TO        NUM-FLG.
     PERFORM     VARYING   X   FROM  1     BY   1
                 UNTIL     X         >     7
                 IF        NUM-FLG   =     ZERO
                     IF    WK-DEN172A(X:1) NOT =     "0"
                           MOVE   1  TO    NUM-FLG
                     END-IF
                 END-IF
*
                 MOVE      WK-DEN172A(X:1) TO   BD28(I)(Y:1)
                 IF        NUM-FLG       = ZERO
                     IF    BD28(I)(Y:1)  = ZERO
                           MOVE   SPACE  TO     BD28(I)(Y:1)
                     END-IF
                 END-IF
*
                 IF        Z         =     1
                           ADD       1     TO   Y
                           MOVE      2     TO   Z
                 ELSE
                           ADD       2     TO   Y
                           MOVE      1     TO   Z
                 END-IF
     END-PERFORM.
     ADD         W-GENKA   TO        G-GENKA.
     ADD         W-BAIKA   TO        G-BAIKA.
 BODY-EDT-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-EDT-SEC                  SECTION.
     MOVE        WK-BIKO1  TO        BIKO1.
**** DISPLAY "WK-BIKO1-1 = " WK-BIKO1 UPON CONS.
**** DISPLAY "BIKO1      = " BIKO1    UPON CONS.
     MOVE        WK-BIKO2  TO        BIKO2.
     MOVE        G-GENKA   TO        WK-DEN172.
     MOVE        SPACE     TO        TL01.
     MOVE        1         TO        Y.
     MOVE        1         TO        Z.
     MOVE        ZERO      TO        NUM-FLG.
     PERFORM     VARYING   X   FROM  1     BY   1
                 UNTIL     X         >     7
                 IF        NUM-FLG   =     ZERO
                     IF    WK-DEN172A(X:1) NOT =     "0"
                           MOVE   1  TO    NUM-FLG
                     END-IF
                 END-IF
*
                 MOVE      WK-DEN172A(X:1) TO   TL01(Y:1)
                 IF        NUM-FLG       = ZERO
                     IF    TL01   (Y:1)  = ZERO
                           MOVE   SPACE  TO     TL01(Y:1)
                     END-IF
                 END-IF
*
                 IF        Z         =     1
                           ADD       2     TO   Y
                           MOVE      2     TO   Z
                 ELSE
                           ADD       1     TO   Y
                           MOVE      1     TO   Z
                 END-IF
     END-PERFORM.
*
     MOVE        G-BAIKA   TO        WK-DEN172.
     MOVE        SPACE     TO        TL02.
     MOVE        1         TO        Y.
     MOVE        1         TO        Z.
     MOVE        ZERO      TO        NUM-FLG.
     PERFORM     VARYING   X   FROM  1     BY   1
                 UNTIL     X         >     7
                 IF        NUM-FLG   =     ZERO
                     IF    WK-DEN172A(X:1) NOT =     "0"
                           MOVE   1  TO    NUM-FLG
                     END-IF
                 END-IF
*
                 MOVE      WK-DEN172A(X:1) TO   TL02(Y:1)
                 IF        NUM-FLG       = ZERO
                     IF    TL02   (Y:1)  = ZERO
                           MOVE   SPACE  TO     TL02(Y:1)
                     END-IF
                 END-IF
*
                 IF        Z         =     1
                           ADD       2     TO   Y
                           MOVE      2     TO   Z
                 ELSE
                           ADD       1     TO   Y
                           MOVE      1     TO   Z
                 END-IF
     END-PERFORM.
     MOVE        ZERO      TO        G-GENKA.
     MOVE        ZERO      TO        G-BAIKA.
 TAIL-EDT-EXIT.
     EXIT.
**********************************************************
*                 データ編集書き出し               *
**********************************************************
 DENP-WRT-SEC                  SECTION.
**************
*帳票書き出し*
**************
     MOVE       "FTE16011" TO        PRT-FORM.
     MOVE       "ALLF"     TO        PRT-GRP.
     WRITE       FTE16011.
*帳票初期化*
     MOVE        SPACE     TO        FTE16011.
     MOVE        ZERO      TO        I.
 DENP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*
     MOVE     SPACE     TO        FORM-PARA.
     MOVE     WK-MAI    TO        DENMAI.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FTE16012" TO        DSP-FMT.
     WRITE    FTE16012
*
     MOVE     SPACE     TO        ERRMSG.
 DSP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                      2.3        *
****************************************************************
 DSP-RD-SEC           SECTION.
*
     MOVE    "NE"       TO        DSP-PRO.
     READ     DSPF.
*
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理                            2.4      *
****************************************************************
 KEY-IN-SEC             SECTION.
 KEY-IN-01.
     MOVE         "NE"       TO   DSP-PRO.
     MOVE         "KEY01"    TO   DSP-GRP.
     PERFORM       DSP-RD-SEC.
*
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE      9         TO   END-FLG
                   GO                  TO   KEY-IN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      MSG04     TO   ERRMSG
                   GO                  TO   KEY-IN-EXIT
     END-EVALUATE.
****************
*エラーチェック*
****************
*
     IF  (KAIDEN  NOT  NUMERIC)
         MOVE    ZERO      TO        KAIDEN.
*
     IF  (ENDDEN  NOT  NUMERIC)
     OR  (ENDDEN = ZERO)
         MOVE  ALL "9"     TO        ENDDEN.
*出力区分チェック
     IF  (KUBUN  =  "1"  OR  "2"  OR  "3")
         MOVE   "M"        TO        EDIT-OPTION  OF    KUBUN
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KUBUN
       ELSE
         MOVE   "R"        TO        EDIT-OPTION  OF    KUBUN
         MOVE   "C"        TO        EDIT-CURSOR  OF    KUBUN
         MOVE    MSG02     TO        ERRMSG
         PERFORM DSP-WRT-SEC
         GO                TO        KEY-IN-01.
*伝票Ｎｏ大小チェック
     IF  (KAIDEN > ENDDEN)
         MOVE   "R"        TO        EDIT-OPTION  OF    KAIDEN
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDDEN
         MOVE   "C"        TO        EDIT-CURSOR  OF    KAIDEN
         MOVE    MSG05     TO        ERRMSG
         PERFORM DSP-WRT-SEC
         GO                TO        KEY-IN-01
       ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF    KAIDEN
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDDEN
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KAIDEN
     END-IF.
*項目表示
     PERFORM     DSP-WRT-SEC.
 KEY-IN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 DEN-READ-SEC            SECTION.
*
 READ-000.
     READ     HDENJNL   AT        END
              MOVE      1         TO   END-FLG
              MOVE      MSG01     TO   ERRMSG
              GO                  TO   DEN-READ-EXIT
     END-READ.
*
     IF       DEN-F01  >   72028
              MOVE      1         TO   END-FLG
              GO                  TO   DEN-READ-EXIT
     END-IF.
     IF      (DEN-F02  <   KAIDEN)
              GO                  TO   READ-000
     END-IF.
     IF      (DEN-F02  >   ENDDEN)
              MOVE      1         TO   END-FLG
              GO                  TO   DEN-READ-EXIT
     END-IF.
*
     IF      (WK-DEN  =   ZERO)
              PERFORM   HEAD-EDT-SEC
              MOVE      DEN-F02   TO   WK-DEN.
 READ-010.
     IF       DEN-F02   NOT =     WK-DEN
              PERFORM   TAIL-EDT-SEC
              PERFORM   DENP-WRT-SEC
              PERFORM   HEAD-EDT-SEC
              MOVE      DEN-F02   TO   WK-DEN
     END-IF.
*
     IF       DEN-F03   <    80
              PERFORM  BODY-EDT-SEC
     ELSE
              IF    DEN-F03  =  80
                    MOVE   DEN-F1421  TO    WK-BIKO1
                    MOVE   DEN-F1422  TO    WK-BIKO2
              END-IF
     END-IF.
*
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*
     PERFORM   VARYING   I   FROM  1  BY  1   UNTIL   I  >  6

          MOVE   ALL "9"         TO   BD23(I) BD24(I) BD25(I)
                                      BD26(I) BD27(I) BD28(I)
          MOVE   ALL "*"         TO   BD11(I) BD21(I) BD22(I)

     END-PERFORM.
*
*
     MOVE   ALL "*"              TO   HD01    HD03.
*
*伝票発行
     PERFORM  DENP-WRT-SEC.
*
     MOVE       1                TO   END-FLG.
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             手書売上伝票ファイルスタート          2.6        *
****************************************************************
 DEN-START-SEC          SECTION.
*
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE            TO  DEN-REC.
     INITIALIZE  DEN-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     72028              TO   DEN-F01.
     MOVE     KAIDEN             TO   DEN-F02.
     MOVE     ZERO               TO   DEN-F04.
     MOVE     40                 TO   DEN-F051.
     MOVE     ZERO               TO   DEN-F03.
*  2011/10/05,S  S.I/NAV
**     START    HDENJNL   KEY  IS  >=   DEN-F01  DEN-F02  DEN-F04
**                                      DEN-F051 DEN-F03
**              INVALID
**              DISPLAY  "ﾃﾞﾝﾋﾟｮｳ ﾃﾞｰﾀ  ﾅｼ!!"  UPON  CONS
**              ACCEPT   IN-DATA               FROM  CONS
**              STOP  RUN
**     END-START.
     START    HDENJNL   KEY  IS  >=   DEN-F01  DEN-F02  DEN-F04
                                      DEN-F051 DEN-F07  DEN-F112
                                      DEN-F03
              INVALID
              DISPLAY  "ﾃﾞﾝﾋﾟｮｳ ﾃﾞｰﾀ  ﾅｼ!!"  UPON  CONS
              ACCEPT   IN-DATA               FROM  CONS
              STOP  RUN
     END-START.
*  2011/10/05,E  S.I/NAV
*
 DEN-START-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  STE1601B.
******************************************************************

```
