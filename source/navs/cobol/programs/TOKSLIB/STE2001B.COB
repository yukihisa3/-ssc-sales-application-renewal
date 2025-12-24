****************************************************************
*   SYSTEM ･･･ケーヨーオンライン　システム
*   PG-NAME･･･手書き伝票（新手書伝票）ＪＡＮ対応
*   PG-ID  ･･･S T E 2 0 0 1 B
*   更新履歴 ：
*     2011/10/05 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE2001B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/08/10.
******************************************************************
 ENVIRONMENT            DIVISION.
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   STATION.
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
*取引先マスタ
     SELECT   HTOKMS         ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   RANDOM
                             RECORD    KEY  IS   TOK-F01
                             FILE STATUS    IS   TOK-ST.
*店舗マスタ
     SELECT   HTENMS         ASSIGN    TO        DA-01-VI-TENMS1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   RANDOM
                             RECORD    KEY  IS   TEN-F52
                                                 TEN-F011
                             FILE STATUS    IS   TEN-ST.
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
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*店舗マスタ
 FD  HTENMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     BLOCK       CONTAINS  24        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE20011    OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE20012   OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*** ｽﾃｰﾀｽ
 01  ST-AREA.
     03  DEN-ST                   PIC  X(02).
     03  DEN-ST1                  PIC  X(04).
     03  TOK-ST                   PIC  X(02).
     03  TOK-ST1                  PIC  X(04).
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
     03  WK-NEW-FLG               PIC  9(04)     VALUE   ZERO.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
     03  WK-DENPYO                PIC  9(09)     VALUE   ZERO.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  G-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  G-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
*店舗コード
 01  WK-DEN07                     PIC  9(05).
 01  WK-DEN07R     REDEFINES      WK-DEN07.
     03  WK-DEN07X                PIC  9(02).
     03  WK-DEN07A                PIC  9(01).
     03  WK-DEN07B                PIC  9(01).
     03  WK-DEN07C                PIC  9(01).
*納品日
 01  WK-DEN112                    PIC  9(08).
 01  WK-DEN112R    REDEFINES      WK-DEN112.
     03  WK-DEN112YY              PIC  9(02).
     03  WK-DEN112Y               PIC  9(02).
     03  WK-DEN112M               PIC  9(02).
     03  WK-DEN112D               PIC  9(02).
*取引先コード
 01  WK-DEN01                     PIC  9(08).
 01  WK-DEN01R     REDEFINES      WK-DEN01.
     03  WK-DEN01X                PIC  9(04).
     03  WK-DEN01A                PIC  9(01).
     03  WK-DEN01B                PIC  9(01).
     03  WK-DEN01C                PIC  9(01).
     03  WK-DEN01D                PIC  9(01).
*数量
 01  WK-DEN15                     PIC  ZZZZ.
 01  WK-DEN15R     REDEFINES      WK-DEN15       PIC  X(04).
*原単価
 01  WK-DEN172                    PIC  ZZZZZ.
 01  WK-DEN172R    REDEFINES      WK-DEN172      PIC  X(05).
*原価金額
 01  WK-DEN181                    PIC  ZZZZZZ.
 01  WK-DEN181R    REDEFINES      WK-DEN181      PIC  X(06).
*売単価
 01  WK-DEN173                    PIC  ZZZZZ.
 01  WK-DEN173R    REDEFINES      WK-DEN173      PIC  X(05).
*売価金額
 01  WK-URIKIN                    PIC  ZZZZZZ.
 01  WK-URIKINR    REDEFINES      WK-URIKIN      PIC  X(06).
*注文日
 01  WK-DEN111                    PIC  9(08).
 01  WK-DEN111R    REDEFINES      WK-DEN111.
     03  WK-DEN111YY              PIC  9(02).
     03  WK-DEN111Y               PIC  9(02).
     03  WK-DEN111M               PIC  9(02).
     03  WK-DEN111D               PIC  9(02).
******  4/20  *****
*注文■
 01  WK-DEN10                     PIC  X(07).
******  4/20  *****
*原価金額合計
 01  WG-GENKA                     PIC  ZZZZZZZZZ.
 01  WG-GENKAR     REDEFINES      WG-GENKA       PIC  X(09).
*売価金額合計
 01  WG-URIKIN                    PIC  ZZZZZZZZZ.
 01  WG-URIKINR    REDEFINES      WG-URIKIN      PIC  X(09).
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
         NC"　　　　　　　　　　　　　　　".
     03  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください".
     03  MSG03                PIC  N(30)     VALUE
         NC"『手書き伝票発行中』".
     03  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG05                PIC  N(30)     VALUE
         NC"開始が終了より大きいです".
*
 01  DEN-ERR                      PIC  N(11)     VALUE
         NC"伝票データ　異常！！".
 01  TOK-ERR                      PIC  N(11)     VALUE
         NC"取引先マスタ異常！！".
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
     DISPLAY  DEN-ERR          UPON    STATION.
     DISPLAY  DEN-ST           UPON    STATION.
     ACCEPT   IN-DATA          FROM    STATION.
     MOVE     4010             TO      PROGRAM-STATUS.
     STOP     RUN.
*取引先マスタ
 TOK-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HTOKMS.
     DISPLAY  TOK-ERR          UPON    STATION.
     DISPLAY  TOK-ST           UPON    STATION.
     ACCEPT   IN-DATA          FROM    STATION.
     MOVE     4010             TO      PROGRAM-STATUS.
     STOP     RUN.
*店舗マスタ
 TEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HTENMS.
     DISPLAY  TEN-ERR          UPON    STATION.
     DISPLAY  TEN-ST           UPON    STATION.
     ACCEPT   IN-DATA          FROM    STATION.
     MOVE     4010             TO      PROGRAM-STATUS.
     STOP     RUN.
*条件ファイル
 JYO-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HJYOKEN.
     DISPLAY  JYO-ERR          UPON    STATION.
     DISPLAY  JYO-ST           UPON    STATION.
     ACCEPT   IN-DATA          FROM    STATION.
     MOVE     4010             TO      PROGRAM-STATUS.
     STOP     RUN.
*プリンター
 PRT-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTF.
     DISPLAY  PRT-ERR          UPON    STATION.
     DISPLAY  PRT-ST           UPON    STATION.
     ACCEPT   IN-DATA          FROM    STATION.
     MOVE     4010             TO      PROGRAM-STATUS.
     STOP     RUN.
*画面ファイル
 DSP-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPF.
     DISPLAY  DSP-ERR          UPON    STATION.
     DISPLAY  DSP-ST           UPON    STATION.
     ACCEPT   IN-DATA          FROM    STATION.
     MOVE     4010             TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 PROC-SEC                  SECTION.
     OPEN        I-O       DSPF.
 PROC-010.
     MOVE        SPACE     TO        FTE20012.
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
                           HTOKMS
                           HTENMS
                           HJYOKEN
                 OUTPUT    PRTF.
*
     MOVE     ZERO               TO   WK-DEN.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     ZERO               TO   WK-MAI.
     MOVE     ZERO               TO   WK-DENPYO.
*    売上伝票ファイルスタート
     PERFORM  DEN-START-READ-SEC.
     IF       END-FLG  =  9
              DISPLAY NC"出力対象無し" UPON STATION
              STOP  RUN
     END-IF.
 INIT001.
*伝票総枚数読込み
     READ     HDENJNL  AT  END
         IF   WK-MAI  =  ZERO
              DISPLAY NC"出力対象無し" UPON STATION
              STOP  RUN
         ELSE
              GO                 TO   INIT002
         END-IF
     END-READ.
 INIT0011.
*ケーヨー以外読み飛ばし
     IF       DEN-F01     >    173
         IF   WK-MAI  =  ZERO
              DISPLAY NC"出力対象無し" UPON STATION
              STOP  RUN
         ELSE
              GO                 TO   INIT002
         END-IF
     END-IF.
 INIT0012.
*売場コードチェック
*****PERFORM  TENPO-RD-SEC.
*売場コードゼロ以外は読み飛ばし
*****IF       WK-NEW-FLG  NOT =  1
*****         GO                 TO   INIT001
*****END-IF.
**** DISPLAY "DEN-F27F = " DEN-F27F UPON STATION.
     IF       DEN-F27F    NOT =  1
              GO                 TO   INIT001
     END-IF.
 INIT0013.
*行が８０以上は読み飛ばし
     IF       DEN-F03     >=     80
              GO                 TO   INIT001
     END-IF.
 INIT0014.
*伝票■ブレイクチェック
     IF       DEN-F02  NOT =  WK-DENPYO
              ADD       1        TO   WK-MAI
              MOVE      DEN-F02  TO   WK-DENPYO
     END-IF.
*****DISPLAY "WK-MAI    = " WK-MAI    UPON STATION.
     GO                          TO   INIT001.
*
 INIT002.
*****MOVE     DEN-F98            TO   WK-MAI.
     CLOSE    HDENJNL.
*
*****OPEN     INPUT     HDENJNL.
************
*帳票初期化*
************
     MOVE        SPACE     TO        FTE20011.
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
                   OPEN INPUT HDENJNL
                   PERFORM   TEST-PRT-SEC
                   CLOSE    HDENJNL
         WHEN      "2"
                   OPEN INPUT HDENJNL
                   MOVE      ZERO      TO   I
                   MOVE      ZERO      TO   KAIDEN
                   MOVE      ALL "9"   TO   ENDDEN
                   PERFORM   DEN-START-READ-SEC
                   IF        END-FLG  NOT =  9
                             PERFORM   DEN-READ-SEC
                                       UNTIL     END-FLG   =   1
                   END-IF
                   CLOSE    HDENJNL
         WHEN      "3"
                   OPEN INPUT HDENJNL
                   MOVE      ZERO      TO   I
                   PERFORM   KEY-IN-SEC
                   PERFORM   DEN-START-READ-SEC
                   IF        END-FLG  NOT =  9
                             PERFORM   DEN-READ-SEC
                                       UNTIL     END-FLG   =   1
                   END-IF
                   CLOSE    HDENJNL
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
     CLOSE    HTOKMS  HTENMS           HJYOKEN  PRTF.
*****CLOSE    HTOKMS  HTENMS  HDENJNL  HJYOKEN  PRTF.
*****CLOSE    HTOKMS          HDENJNL  HJYOKEN  PRTF.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-EDT-SEC                 SECTION.
*店名（上段）
     MOVE        DEN-F01     TO      TOK-F01.
     READ        HTOKMS
       INVALID
         MOVE    ALL "*"     TO      W001
       NOT INVALID
         MOVE    TOK-F04     TO      W001
     END-READ.
*ニック店舗は、取引先名変更
     IF  DEN-F07  =  211
     OR  DEN-F07  =  212
     OR  DEN-F07  =  213
     OR  DEN-F07  =  214
     OR  DEN-F07  =  215
     OR  DEN-F07  =  216
     OR  DEN-F07  =  217
     OR  DEN-F07  =  218
     OR  DEN-F07  =  219
     OR  DEN-F07  =  220
     OR  DEN-F07  =  221
     OR  DEN-F07  =  222
         MOVE   "ﾆｯｸﾎﾋﾞｰｼｮｯﾌﾟ" TO    W001
     END-IF.
*店名（下段）
*    MOVE        DEN-F01     TO      TEN-F52.
*    MOVE        DEN-F07     TO      TEN-F011.
*    READ        HTENMS
*       INVALID
*        MOVE    ALL "*"     TO      W002
*      NOT INVALID
*        MOVE    TEN-F04     TO      W002
*    END-READ.
     MOVE        DEN-F30     TO      W002.
*店コード
     MOVE        DEN-F07     TO      WK-DEN07.
     MOVE        WK-DEN07A   TO      W003A.
     MOVE        WK-DEN07B   TO      W003B.
     MOVE        WK-DEN07C   TO      W003C.
*部門コード
     MOVE        DEN-F12(1:2)        TO      W004.
*納品日
     MOVE        DEN-F112    TO      WK-DEN112.
     MOVE        WK-DEN112Y  TO      W005Y.
     MOVE        WK-DEN112M  TO      W005M.
     MOVE        WK-DEN112D  TO      W005D.
*取引先コード
*94.12.20 START
*****IF          DEN-F132    =       "99"
*****     MOVE   760         TO      WK-DEN01
*****ELSE
*****     MOVE   DEN-F01     TO      WK-DEN01
*****END-IF.
*## 2001/04/05 NAV START ##
*****DISPLAY "DEN-F132 = " DEN-F132 UPON STATION.
     EVALUATE    DEN-F132
         WHEN    "99"  MOVE  0760    TO  WK-DEN01
         WHEN    "55"  MOVE  1455    TO  WK-DEN01
         WHEN    "60"  MOVE  1460    TO  WK-DEN01
         WHEN    "73"  MOVE  1473    TO  WK-DEN01
         WHEN    "47"  MOVE  1647    TO  WK-DEN01
         WHEN    OTHER MOVE  DEN-F01 TO  WK-DEN01
     END-EVALUATE.
     MOVE        WK-DEN01A   TO      W006A.
     MOVE        WK-DEN01B   TO      W006B.
     MOVE        WK-DEN01C   TO      W006C.
     MOVE        WK-DEN01D   TO      W006D.
*取引先名
     MOVE        12        TO        JYO-F01.
     MOVE        SPACE     TO        JYO-F02.
     READ        HJYOKEN
       INVALID
         MOVE    ALL "*"          TO        W007
       NOT INVALID
         MOVE    JYO-F03          TO        W007
     END-READ.
*備考，ストック■
*2008/10/29 出力しない様に変更
*****MOVE        DEN-F22          TO        W020.
     MOVE        SPACE            TO        W020.
     MOVE        DEN-F21          TO        W022.
*伝票■
*99/08/11 伝票■は表示しない
*****MOVE        DEN-F02(1:2)     TO        DEN01.
*****MOVE        "-"              TO        W003.
*****MOVE        DEN-F02(3:3)     TO        DEN02.
*****MOVE        "-"              TO        W005.
*****MOVE        DEN-F02(6:4)     TO        DEN03.
*伝票■（下４桁）
     MOVE        DEN-F23          TO        W023.
 HEAD-EDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 BODY-EDT-SEC                  SECTION.
     ADD         1             TO      I.
*商品名，コード
     MOVE        DEN-F1421     TO      W008(I).
     MOVE        DEN-F1422     TO      W009(I).
     MOVE        DEN-F25       TO      W010(I).
*数量
     MOVE        DEN-F15       TO      WK-DEN15.
     MOVE        WK-DEN15(1:1) TO      W11A(I).
     MOVE        WK-DEN15(2:1) TO      W11B(I).
     MOVE        WK-DEN15(3:1) TO      W11C(I).
     MOVE        WK-DEN15(4:1) TO      W11D(I).
*原単価
     MOVE        DEN-F172       TO      WK-DEN172.
     MOVE        WK-DEN172(1:1) TO      W12A(I).
     MOVE        WK-DEN172(2:1) TO      W12B(I).
     MOVE        WK-DEN172(3:1) TO      W12C(I).
     MOVE        WK-DEN172(4:1) TO      W12D(I).
     MOVE        WK-DEN172(5:1) TO      W12E(I).
*原価金額
     MOVE        DEN-F181       TO      WK-DEN181.
     MOVE        WK-DEN181(1:1) TO      W13A(I).
     MOVE        WK-DEN181(2:1) TO      W13B(I).
     MOVE        WK-DEN181(3:1) TO      W13C(I).
     MOVE        WK-DEN181(4:1) TO      W13D(I).
     MOVE        WK-DEN181(5:1) TO      W13E(I).
     MOVE        WK-DEN181(6:1) TO      W13F(I).
*売単価
     MOVE        DEN-F173       TO      WK-DEN173.
     MOVE        WK-DEN173(1:1) TO      W14A(I).
     MOVE        WK-DEN173(2:1) TO      W14B(I).
     MOVE        WK-DEN173(3:1) TO      W14C(I).
     MOVE        WK-DEN173(4:1) TO      W14D(I).
     MOVE        WK-DEN173(5:1) TO      W14E(I).
*売価金額
     COMPUTE     W-BAIKA  =  DEN-F15  *  DEN-F173.
     MOVE        W-BAIKA        TO      WK-URIKIN.
     MOVE        WK-URIKIN(1:1) TO      W15A(I).
     MOVE        WK-URIKIN(2:1) TO      W15B(I).
     MOVE        WK-URIKIN(3:1) TO      W15C(I).
     MOVE        WK-URIKIN(4:1) TO      W15D(I).
     MOVE        WK-URIKIN(5:1) TO      W15E(I).
     MOVE        WK-URIKIN(6:1) TO      W15F(I).
     ADD         DEN-F181       TO      G-GENKA.
     ADD         W-BAIKA        TO      G-BAIKA.
*****  4/20  *****
*注文■退避
*****MOVE        DEN-F10     TO        WK-DEN10.
     MOVE        DEN-F44(9:7) TO       WK-DEN10.
*注文日退避
     MOVE        DEN-F111    TO        WK-DEN111.
*****  4/20  *****
 BODY-EDT-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-EDT-SEC                  SECTION.
*注文■
*4/20MOVE        DEN-F10     TO        W016.
     MOVE        WK-DEN10    TO        W016.
*注文日
*4/20MOVE        DEN-F111    TO        WK-DEN111.
     MOVE        WK-DEN111Y  TO        W017Y.
     MOVE        WK-DEN111M  TO        W017M.
     MOVE        WK-DEN111D  TO        W017D.
*金額合計
     MOVE        G-GENKA       TO      WG-GENKA.
     MOVE        WG-GENKA(1:1) TO      W018A.
     MOVE        WG-GENKA(2:1) TO      W018B.
     MOVE        WG-GENKA(3:1) TO      W018C.
     MOVE        WG-GENKA(4:1) TO      W018D.
     MOVE        WG-GENKA(5:1) TO      W018E.
     MOVE        WG-GENKA(6:1) TO      W018F.
     MOVE        WG-GENKA(7:1) TO      W018G.
     MOVE        WG-GENKA(8:1) TO      W018H.
     MOVE        WG-GENKA(9:1) TO      W018I.
*売価金額
     MOVE        G-BAIKA        TO     WG-URIKIN.
     MOVE        WG-URIKIN(1:1) TO     W019A.
     MOVE        WG-URIKIN(2:1) TO     W019B.
     MOVE        WG-URIKIN(3:1) TO     W019C.
     MOVE        WG-URIKIN(4:1) TO     W019D.
     MOVE        WG-URIKIN(5:1) TO     W019E.
     MOVE        WG-URIKIN(6:1) TO     W019F.
     MOVE        WG-URIKIN(7:1) TO     W019G.
     MOVE        WG-URIKIN(8:1) TO     W019H.
     MOVE        WG-URIKIN(9:1) TO     W019I.
*
     MOVE        ZERO          TO      G-GENKA.
     MOVE        ZERO          TO      G-BAIKA.
 TAIL-EDT-EXIT.
     EXIT.
**********************************************************
*                 データ編集書き出し               *
**********************************************************
 DENP-WRT-SEC                  SECTION.
**************
*帳票書き出し*
**************
     MOVE       "FTE20011"   TO        PRT-FORM.
     MOVE       "ALLF"     TO        PRT-GRP.
     WRITE       FTE20011.
*帳票初期化*
     MOVE        SPACE     TO        FTE20011.
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
     MOVE    "FTE20012"  TO        DSP-FMT.
     WRITE    FTE20012.
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
*伝票■大小チェック
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
*ケーヨー以外読み飛ばし
     IF       DEN-F01     >    173
              MOVE      1         TO   END-FLG
              MOVE      MSG01     TO   ERRMSG
              GO                  TO   DEN-READ-EXIT
     END-IF.
*売場コードが１以外の場合
*****PERFORM  TENPO-RD-SEC.
*****IF       WK-NEW-FLG  NOT =  1
*****         GO                  TO   READ-000
*****END-IF.
     IF       DEN-F27F    NOT =  1
              GO                  TO   READ-000
     END-IF.
*行が８０以上は読み飛ばし1999/08/18 PG UPDATE TAKAHASHI
     IF       DEN-F03     >      80
              GO                 TO   READ-000
     END-IF.
*
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
*
     IF      (DEN-F03  =   80)
              MOVE      DEN-F1421 TO   SHO1
              MOVE      DEN-F1422 TO   SHO2
              GO                  TO   READ-000.
*
     IF      (DEN-F03  =   90)
              GO                  TO   READ-000.
 READ-010.
     IF       DEN-F02   NOT =     WK-DEN
              PERFORM   TAIL-EDT-SEC
              PERFORM   DENP-WRT-SEC
              PERFORM   HEAD-EDT-SEC
              MOVE      DEN-F02   TO   WK-DEN
     END-IF.
*
     PERFORM  BODY-EDT-SEC.
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*
     PERFORM   VARYING   I   FROM  1  BY  1   UNTIL   I  >  6

          MOVE   ALL "9"         TO   W11A(I) W11B(I) W11C(I)
                                      W11D(I) W12A(I) W12B(I)
                                      W12C(I) W12D(I) W12E(I)
                                      W13A(I) W13B(I) W13C(I)
                                      W13D(I) W13E(I) W13F(I)
                                      W14A(I) W14B(I) W14C(I)
                                      W14D(I) W14E(I)
                                      W15A(I) W15B(I) W15C(I)
                                      W15D(I) W15E(I) W15F(I)
          MOVE   ALL "*"         TO   W008(I) W009(I) W010(I)
     END-PERFORM.
*
     MOVE   ALL "9"              TO   W003A   W003B   W003C
                                      W005Y   W005M   W005D
                                      W006A   W006B
                                      W006C   W006D
                                      W017Y   W017M   W017D
                                      W018A   W018B   W018C
                                      W018D   W018E   W018F
                                      W018G   W018H   W018I
                                      W019A   W019B   W019C
                                      W019D   W019E   W019F
                                      W019G   W019H   W019I
                                      W004    W023.
*
     MOVE   ALL "*"              TO   W001    W002    W020
                                      W022    W016    SHO1
                                      SHO2.
*
     MOVE   ALL NC"＊"           TO   W007.
*伝票発行
     PERFORM  DENP-WRT-SEC.
*
     MOVE          1             TO   END-FLG.
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             店舗マスタ検索                                   *
****************************************************************
 TENPO-RD-SEC           SECTION.
*
     MOVE        DEN-F01     TO      TEN-F52.
     MOVE        DEN-F07     TO      TEN-F011.
     READ        HTENMS
        INVALID
         MOVE    ZERO        TO      WK-NEW-FLG
       NOT INVALID
         MOVE    TEN-F79     TO      WK-NEW-FLG
     END-READ.
*
 TENPO-RD-EXIT.
     EXIT.
****************************************************************
*             売上伝票ファイルスタート                         *
****************************************************************
 DEN-START-READ-SEC    SECTION.
*
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE           TO  DEN-REC.
     INITIALIZE  DEN-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     173                TO   DEN-F01.
     MOVE     ZERO               TO   DEN-F02.
     MOVE     ZERO               TO   DEN-F04.
     MOVE     ZERO               TO   DEN-F051.
     MOVE     ZERO               TO   DEN-F03.
*  2011/10/05,S  S.I/NAV
**     START    HDENJNL  KEY  IS  >=  DEN-F01  DEN-F02  DEN-F04
**                                    DEN-F051 DEN-F03
**              INVALID
**              MOVE     9         TO   END-FLG
**     END-START.
     START    HDENJNL  KEY  IS  >=  DEN-F01  DEN-F02  DEN-F04
                                    DEN-F051 DEN-F07  DEN-F112
                                    DEN-F03
              INVALID
              MOVE     9         TO   END-FLG
     END-START.
*  2011/10/05,E  S.I/NAV
*
 DEN-START-READ-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  STE2001B.
******************************************************************
