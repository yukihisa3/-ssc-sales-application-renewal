# SNJ0570L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ0570L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩ受信サーバー                *
*    業務名　　　　　　　：　受信件数                          *
*    モジュール名　　　　：　受信件数リスト                    *
*    作成日／更新日　　　：　10/08/06                          *
*    作成者／更新者　　　：　ＮＡＶ大野                        *
*    処理概要　　　　　　：　出荷箇所別件数ファイルを元に受信件*
*                            数リストの出力を行う。            *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SNJ0570L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          XX/XX/XX.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*出荷場所別件数Ｆ
     SELECT     JSMKENF    ASSIGN    TO        JSMKENL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       KEN-F01
                                               KEN-F02
                                               KEN-F03
                                               KEN-F04
                           FILE      STATUS    KEN-ST.
*ＥＤＩ管理マスタ
     SELECT     JSMEDIF    ASSIGN    TO        JSMEDIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       EDI-F01
                                               EDI-F02
                                               EDI-F03
                           FILE      STATUS    EDI-ST.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO        ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*プリント定義ファイル
     SELECT     PRTFILE    ASSIGN    TO        GS-PRTF
                PROCESSING MODE      IS        PRT-PROC
                GROUP                IS        PRT-GROUP
                FORMAT               IS        PRT-FORMAT
                CONTROL              IS        PRT-CNTL
                FILE STATUS          IS        PRT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
*  FILE=出荷場所別件数Ｆ                                       *
****************************************************************
 FD  JSMKENF
     LABEL       RECORD    IS        STANDARD.
     COPY        JSMKENF   OF        XFDLIB
     JOINING     KEN       AS        PREFIX.
****************************************************************
*  FILE=ＥＤＩ管理マスタ                                       *
****************************************************************
 FD  JSMEDIF
     LABEL       RECORD    IS        STANDARD.
     COPY        JSMEDIF   OF        XFDLIB
     JOINING     EDI       AS        PREFIX.
****************************************************************
*  FILE= 取引先マスタ                                          *
****************************************************************
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
 FD  ZSOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
     COPY        FNJ0570L  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  KEN-ST              PIC X(02).
     03  EDI-ST              PIC X(02).
     03  TOK-ST              PIC X(02).
     03  SOK-ST              PIC X(02).
     03  PRT-ST              PIC X(02).
*帳票制御用領域
 01  PRT-CONTROL.
     03  PRT-PROC            PIC  X(02).
     03  PRT-GROUP           PIC  X(08).
     03  PRT-FORMAT          PIC  X(08).
     03  PRT-CNTL            PIC  X(06).
***  ﾌﾗｸﾞｴﾘｱ
 01  FLG-AREA.
     03  END-FLG             PIC X(03).
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  P-CNT               PIC 9(05) VALUE  ZERO.
     03  WK-TORICD           PIC 9(08) VALUE  ZERO.
     03  SYS-DATE            PIC 9(06) VALUE  ZERO.
     03  IX                  PIC 9(02) VALUE  ZERO.
     03  IY                  PIC 9(02) VALUE  ZERO.
     03  WK-DTKENSU          PIC 9(05) VALUE  ZERO.
     03  WK-DENMAI           PIC 9(05) VALUE  ZERO.
     03  WK-SOUKEN           PIC 9(05) VALUE  ZERO.
*
 01  SEC-NAME.
     03  FILLER              PIC  X(05)     VALUE " *** ".
     03  S-NAME              PIC  X(30).
*
 01  FILE-ERR.
     03  KEN-ERR             PIC N(15) VALUE
                        NC"出荷場所別件数Ｆエラー".
     03  EDI-ERR             PIC N(15) VALUE
                        NC"ＥＤＩ管理マスタエラー".
     03  TOK-ERR             PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR             PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  PRT-ERR             PIC N(15) VALUE
                        NC"プリンターエラー".
***  日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1        PIC  9(02).
     03  WK-DATE8-YY2        PIC  9(06).
 01  WK-DATE8-R         REDEFINES  WK-DATE8.
     03  WK-YYYY             PIC  9(04).
     03  WK-MM               PIC  9(02).
     03  WK-DD               PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-DATE               PIC  9(08).
 01  LINK-TIME               PIC  9(04).
 01  LINK-TORICD             PIC  X(08).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LINK-DATE LINK-TIME LINK-TORICD.
 DECLARATIVES.
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKENF.
     DISPLAY     KEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 EDI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMEDIF.
     DISPLAY     EDI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     EDI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 PROCESS-START             SECTION.
     MOVE        "PROCESS-START"     TO          S-NAME.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG = "END"
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
****************************************************************
*             初期処理                                1.0      *
****************************************************************
 INIT-SEC                  SECTION.
     MOVE        "INIT-SEC"          TO          S-NAME.
*ファイルＯＰＥＮ
 INIT001.
     OPEN        INPUT       JSMKENF
                             JSMEDIF
                             HTOKMS
                             ZSOKMS.
     OPEN        OUTPUT      PRTFILE.
     MOVE        SPACE       TO   PRT-FNJ0570L.
*ヘッダ部出力（日付）
 INIT002.
*項目設定
*フォームID
     MOVE     "FNJ0570L"          TO   PRT-FORM.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
*
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
*
     MOVE     WK-YYYY             TO        PRT-DATEYY.
     MOVE     WK-MM               TO        PRT-DATEMM.
     MOVE     WK-DD               TO        PRT-DATEDD.
***  ページ_
     MOVE     1                   TO        PRT-PAGE.
*
*
*ファイル位置付け
 INIT003.
     MOVE     LINK-DATE           TO        KEN-F01.
     MOVE     LINK-TIME           TO        KEN-F02.
     MOVE     LINK-TORICD         TO        KEN-F03.
 INIT004.
     DISPLAY "LINK-DATE   = " LINK-DATE   UPON CONS.
     DISPLAY "LINK-TIME   = " LINK-TIME   UPON CONS.
     DISPLAY "LINK-TORICD = " LINK-TORICD UPON CONS.
     MOVE     SPACE               TO        KEN-F04.
     START    JSMKENF   KEY  IS   >=        KEN-F01
                                            KEN-F02
                                            KEN-F03
                                            KEN-F04
     INVALID
              MOVE      "END"     TO        END-FLG
              GO   TO   INIT-EXIT
     END-START.
*ファイルＲＥＡＤ
 INIT005.
     PERFORM     FL-READ-SEC.
*ヘッダ部項目転送
     PERFORM     HEAD-WT-SEC.
*フラグの初期化
     MOVE        SPACE       TO   FLG-AREA.
     MOVE        SPACE       TO   END-FLG.
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC                  SECTION.
     MOVE          "MAIN-SEC"     TO        S-NAME.
*
     IF   END-FLG  NOT =  "END"
          PERFORM       BODY-WT-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                    2.1      *
****************************************************************
 FL-READ-SEC            SECTION.
     MOVE     "FL-READ-SEC"       TO             S-NAME.
*
     READ        JSMKENF   AT        END
         MOVE    "END"     TO        END-FLG
         GO                TO        FL-READ-EXIT
     END-READ.
*
     IF  LINK-DATE    NOT =  KEN-F01
     OR  LINK-TIME    NOT =  KEN-F02
     OR  LINK-TORICD  NOT =  KEN-F03
         MOVE    "END"     TO        END-FLG
     END-IF.
*
 FL-READ-EXIT.
     EXIT.

****************************************************************
*             明細出力処理                            2.2      *
****************************************************************
 BODY-WT-SEC                  SECTION.
     MOVE        "BODY-WT-SEC"      TO          S-NAME.
*出荷場所＝スペース（全社）時，次データＲＥＡＤ　
*****IF       KEN-F04   =  ZERO
     IF       KEN-F04   =  SPACE
              GO  TO    DENP-WT010
     END-IF.
*
*15行以上の時は改ページする
     IF       IX       >=  15
*
*             テール部出力処理（プリント出力連動）
              PERFORM  TAIL-WT-SEC
*
*             頁のカウントアップ
              ADD      1              TO   PRT-PAGE
*             明細部、テイル部の初期化
              PERFORM  MEI-INT-SEC
              PERFORM  TAIL-INT-SEC
*             行数の初期化処理
              MOVE     ZERO           TO   IX
     END-IF.
*
     ADD      1    TO   IX.
***  倉庫コード
              MOVE      KEN-F04        TO   PRT-SOKOCD(IX).
***  倉庫名称
              MOVE      KEN-F04        TO   SOK-F01.
              READ      ZSOKMS
              INVALID
                    MOVE   SPACE       TO  PRT-SOKONM(IX)
              NOT INVALID
                    MOVE  SOK-F02      TO  PRT-SOKONM(IX)
              END-READ.
***  ＤＴ件数
              MOVE      KEN-F10        TO   PRT-DTKEN(IX).
***  伝票枚数
              MOVE      KEN-F11        TO   PRT-DENMAI(IX).
*合計計算
     COMPUTE  WK-DTKENSU  =  KEN-F10 + WK-DTKENSU.
     COMPUTE  WK-DENMAI   =  KEN-F11 + WK-DENMAI.
 DENP-WT010.
*
     PERFORM  FL-READ-SEC.
*合計欄出力
     IF   END-FLG  =  "END"
          PERFORM       TAIL-WT-SEC
     END-IF.
*
 BODY-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                        2.3      *
****************************************************************
 HEAD-WT-SEC                  SECTION.
     MOVE        "HEAD-WT-SEC"      TO          S-NAME.
*バッチ_
     MOVE     KEN-F01        TO   PRT-BDATE
     MOVE     KEN-F02        TO   PRT-BTIME
     MOVE     KEN-F03        TO   PRT-BTORI
*取引先名称
     MOVE     KEN-F03        TO   TOK-F01.
     READ     HTOKMS
     INVALID
          MOVE   SPACE       TO   PRT-TORINM
     NOT INVALID
          MOVE   TOK-F03     TO   PRT-TORINM
     END-READ.
*
*回線種別
     MOVE     "01"           TO   EDI-F01.
     MOVE     "1"            TO   EDI-F02.
     MOVE     KEN-F03        TO   EDI-F03.
     READ     JSMEDIF
     INVALID
          MOVE   ZERO        TO   PRT-KAICD
          MOVE   SPACE       TO   PRT-KAINM
     NOT INVALID
          MOVE   EDI-F04     TO   PRT-KAICD
          EVALUATE  EDI-F04
              WHEN  "1"  MOVE   NC"ＩＳＤＮ"   TO   PRT-KAINM
              WHEN  "2"  MOVE   NC"公衆回線"   TO   PRT-KAINM
              WHEN  "3"  MOVE   NC"全銀"       TO   PRT-KAINM
          END-EVALUATE
     END-READ.
*総受信件数　一件目（全社）の件数を表示
     MOVE     KEN-F10        TO   PRT-SOUKEN  WK-SOUKEN.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                        2.4      *
****************************************************************
 TAIL-WT-SEC                  SECTION.
     MOVE        "TAIL-WT-SEC"      TO          S-NAME.
*合計欄セット
***  ＤＴ件数
     MOVE     WK-DTKENSU     TO   PRT-DTKEI.
***  伝票枚数
     MOVE     WK-DENMAI      TO   PRT-DENKEI.
*
     PERFORM  PRT-WT-SEC.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             プリント出力処理                        2.5      *
****************************************************************
 PRT-WT-SEC                  SECTION.
     MOVE     "PRT-WT-SEC"   TO        S-NAME.
*
     MOVE        SPACE       TO   PRT-CONTROL.
     MOVE        "FNJ0570L"  TO   PRT-FORMAT.
     MOVE        "SCREEN"    TO   PRT-GROUP.
     WRITE       PRT-FNJ0570L.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             明細部初期化　　　　                    2.5      *
****************************************************************
 MEI-INT-SEC                  SECTION.
     MOVE     "MEI-INT-SEC"  TO        S-NAME.
*
     PERFORM   VARYING IY FROM 1 BY 1
                          UNTIL IY > 15
               MOVE       SPACE        TO   PRT-MAS001(IY)
*              INITIALIZE                   PRT-MAS001(IY)
     END-PERFORM
*
 MEI-INT-EXIT.
     EXIT.
****************************************************************
*             テール部初期化                         2.5      *
****************************************************************
 TAIL-INT-SEC                  SECTION.
     MOVE     "PRT-WT-SEC"   TO        S-NAME.
*
*    MOVE        SPACE       TO   PRT-DTKEI.
*    MOVE        SPACE       TO   PRT-DENKEI.
     INITIALIZE                   PRT-DTKEI.
     INITIALIZE                   PRT-DENKEI.
*
 TAIL-INT-EXIT.
     EXIT.
****************************************************************
*             終了処理                                3.0      *
****************************************************************
 END-SEC                   SECTION.
     MOVE        "END-SEC"           TO          S-NAME.
*
     CLOSE       PRTFILE  JSMKENF  JSMEDIF  HTOKMS  ZSOKMS.
 END-EXIT.
     EXIT.

```
