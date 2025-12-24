****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　出荷処理　　　　　　　　　　　　　*
*    モジュール名　　　　：　テクノス基本情報データ作成        *
*    作成日／更新日　　　：　2011/12/02                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　手書伝票基本データを読み、基本情報*
*                            Ｆを作成する。　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3741B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/12/02.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENWK  ASSIGN    TO        DA-01-S-NFDENWK
                        FILE      STATUS    DEN-STATUS.
*基本情報ファイル
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JHO-F01   JHO-F05
                                            JHO-F06   JHO-F07
                                            JHO-F08   JHO-F09
                        FILE      STATUS    JHO-STATUS.
*作場マスタ
     SELECT   SAKUBAF   ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SAK-F01
                        STATUS              SAK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    手書伝票基本ファイル
******************************************************************
 FD  SHTDENWK
                        LABEL RECORD   IS   STANDARD.
     COPY     NFDENWK   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    出荷情報ファイル
******************************************************************
 FD  NFJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JHO       PREFIX.
******************************************************************
*    作場マスタ
******************************************************************
 FD  SAKUBAF            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF   OF        XFDLIB
              JOINING   SAK       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP1-CNT           PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  JHO-CNT             PIC  9(08)     VALUE  ZERO.
     03  JYO-CNT             PIC  9(08)     VALUE  ZERO.
     03  KAN-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  NFJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  WK-NOUSU                PIC  9(08)     VALUE  ZERO.
 01  WK-HAISIN.
     03  WK-HACHSHA          PIC  9(04)     VALUE  ZERO.
     03  WK-TORICD           PIC  9(06)     VALUE  ZERO.
     03  FILLER              PIC  X(03)     VALUE  SPACE.
 01  WK-HAISINR     REDEFINES     WK-HAISIN.
     03  WK-HAISINCD         PIC  X(13).
*
*ワーク項目
 01  WK-HACSU              PIC 9(11)         VALUE  ZERO.
 01  WK-GENTAN             PIC 9(13)         VALUE  ZERO.
 01  WK-GENKIN             PIC 9(10)         VALUE  ZERO.
 01  WK-KANRINO            PIC 9(08)         VALUE  ZERO.
 01  WK-CACE               PIC 9(05)V9(01)   VALUE  ZERO.
 01  WK-CACEH              PIC 9(05)         VALUE  ZERO.
 01  WK-HAKOATARI          PIC 9(05)V9(01)   VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
     03  SYS-TIME          PIC 9(08).
 01  SYS-TIMEW.
     03  SYS-TIMEHM        PIC  9(04).
     03  FILER             PIC  9(04).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JHO-STATUS        PIC  X(02).
     03  SAK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3741B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3741B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3741B".
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
*納品予定数量
 01  WK-NOUHIN.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
     03  NOU-F01           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
*****03  NOU-F02           PIC  9(09)   VALUE  ZERO.
     03  NOU-F02           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(11)   VALUE  SPACE.
*納品予定原価金額
 01  WK-KOUMOKU.
     03  WK-TANI           PIC  9(05)V9(01) VALUE  ZERO.
*納品予定原価金額
 01  WK-GENKA.
     03  GEN-F01           PIC  9(07)   VALUE  ZERO.
*****03  FILLER            PIC  X(02)   VALUE  SPACE.
*納品予定原価金額
 01  WK-BAIKA              PIC  9(07)   VALUE  ZERO.
*納品予定日項目セット
 01  WK-A05.
     03  WK-A051           PIC  X(04)   VALUE  SPACE.
     03  WK-A052           PIC  9(08)   VALUE  ZERO.
     03  WK-A053           PIC  9(08)   VALUE  ZERO.
     03  WK-A054           PIC  X(24)   VALUE  ZERO.
     03  WK-A055           PIC  X(09)   VALUE  ZERO.
     03  WK-A056           PIC  X(230)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-BMNCD             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-BMNCD
                                       PARA-TANCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENWK.
     MOVE      "SHTDENWK"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF.
     MOVE      "NFJOHOL1 "   TO   AB-FILE.
     MOVE      JHO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SAKUBAF.
     MOVE      "SAKUBAL1 "   TO   AB-FILE.
     MOVE      SAK-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENWK  SAKUBAF.
     OPEN     I-O       NFJOHOF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     ACCEPT      SYS-TIME  FROM      TIME.
     MOVE        SYS-TIME  TO        SYS-TIMEW.
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
*    売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読込　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC    SECTION.
*
     READ     SHTDENWK
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  SHTDENF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*MAIN010.
*    出荷情報Ｆ出力
     PERFORM   NFJOHOF-WRITE-SEC.
*
*    売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　基本情報Ｆ出力　　　　
****************************************************************
 NFJOHOF-WRITE-SEC     SECTION.
*
     MOVE     "NFJOHOF-WRITE-SEC"  TO  S-NAME.
*初期化
     MOVE      SPACE               TO  JHO-REC.
     INITIALIZE                        JHO-REC.
*存在チェックキーセット
     MOVE      DEN-F68             TO  JHO-F01.
     MOVE      DEN-F71             TO  JHO-F05.
     MOVE      DEN-F07             TO  JHO-F06.
     MOVE      DEN-F02             TO  JHO-F07.
     MOVE      DEN-F03             TO  JHO-F08.
     MOVE      DEN-F112            TO  JHO-F09.
**存在チェック
     READ      NFJOHOF
           INVALID
               MOVE   "INV"        TO  NFJOHOF-INV-FLG
           NOT  INVALID
               MOVE   SPACE        TO  NFJOHOF-INV-FLG
     END-READ.
 CHK010.
*判定
     IF        NFJOHOF-INV-FLG = SPACE
               GO                  TO  NFJOHOF-WRITE-EXIT
     END-IF.
*共通エリア
 CHK020.
*初期化
***  MOVE      SPACE               TO  JHO-REC.
***  INITIALIZE                        JHO-REC.
*管理番号
     MOVE      DEN-F68             TO  JHO-F01.
**バッチ日付
     MOVE      99999999            TO  JHO-F02.
**バッチ時刻
     MOVE      9999                TO  JHO-F03.
**バッチ取引先
     MOVE      DEN-F01             TO  JHO-F04.
*****MOVE      99999999            TO  JHO-F04.
**倉庫コード（作場ＣＤ）
     MOVE      DEN-F71             TO  JHO-F05.
**店舗コード　
     MOVE      DEN-F07             TO  JHO-F06.
**伝票番号
     MOVE      DEN-F02             TO  JHO-F07.
**行番号　
     MOVE      DEN-F03             TO  JHO-F08.
**納品日　
     MOVE      DEN-F112            TO  JHO-F09.
**出荷日　　
     MOVE      DEN-F113            TO  JHO-F10.
**入荷予定日
     MOVE      DEN-F112            TO  JHO-F11.
**直送先ＣＤ
*  作場マスタより直送ＣＤ取得
     MOVE      DEN-F09             TO  SAK-F01.
*
     READ     SAKUBAF
          INVALID  KEY
              MOVE  "1"           TO   JHO-F12
          NOT INVALID  KEY
              MOVE  SAK-F03       TO   JHO-F12
     END-READ.
**相手商品ＣＤ（インストアＣＤ）
     MOVE    DEN-F25              TO   JHO-F13
**相手ＪＡＮＣＤ
     MOVE    DEN-F72              TO   JHO-F14.
**サカタ商品ＣＤ
     MOVE    DEN-F1411            TO   JHO-F15.
**サカタ単品ＣＤ
     MOVE    DEN-F1412            TO   JHO-F16.
**オンライン／手書区分
     MOVE    "2"                  TO  JHO-F17.
**発注数量
     MOVE      DEN-F15             TO  JHO-F19.
**訂正数　
     MOVE      DEN-F15             TO  JHO-F20.
**訂正区分
     MOVE      SPACE               TO  JHO-F23.
**出荷指示作成区分
     MOVE      "1"                 TO  JHO-F24.
**出荷指示作成日付
     MOVE      SYS-DATEW           TO  JHO-F25.
**出荷確定ＤＴ区分
     MOVE      SPACE               TO  JHO-F26.
**出荷確定ＤＴ日付
     MOVE      ZERO                TO  JHO-F27.
**出荷場所
     MOVE      DEN-F08             TO  JHO-F28.
**登録担当者部門
     MOVE      PARA-BMNCD          TO  JHO-F30.
**登録担当者
     MOVE      PARA-TANCD          TO  JHO-F31.
**更新担当者部門
     MOVE      SPACE               TO  JHO-F32.
**更新担当者
     MOVE      SPACE               TO  JHO-F32.
*----------------------------
*  ヘッダレコード項目転送
*----------------------------
**  レコード区分
     MOVE      "B"                 TO  JHO-HE01.
**  データ種別
     MOVE      1                   TO  JHO-HE02.
**  伝票番号　　
     MOVE      DEN-F02             TO  JHO-HE04.
**　社コード
     MOVE      4                   TO  JHO-HE05.
**　店ＣＤ
     MOVE      DEN-F07             TO  JHO-HE06.
**　分類ＣＤ
     MOVE      DEN-F12(1:2)        TO  JHO-HE09.
**　伝票区分
     MOVE      DEN-F132            TO  JHO-HE10.
**　発注日
     MOVE      DEN-F111            TO  JHO-HE11.
**　納品日
     MOVE      DEN-F112            TO  JHO-HE12.
**　取引先ＣＤ
     MOVE      DEN-F01             TO  JHO-HE13.
**　ＳＡ
     MOVE      "00"                TO  JHO-HE14.
**　取引先伝票番号
     MOVE      "0946-21-2011"      TO  JHO-HE18.
**　発注区分
     MOVE      ZERO                TO  JHO-HE19.
**　納品場所
     MOVE      DEN-F69             TO  JHO-HE20.
*----------------------
**　ヘッダオプションラインコード
*----------------------
**　レコード区分
     MOVE      "C"                 TO  JHO-HF01.
**　データ種別　
     MOVE      1                   TO  JHO-HF02.
**　ルート
     MOVE      SPACE               TO  JHO-HF12.
*--------------------
*　明細データ
*--------------------
**　レコード区分
     MOVE      "D"                 TO  JHO-ME01.
**　データ種別　　
     MOVE      1                   TO  JHO-ME02.
**　行番号　　
     MOVE      DEN-F03             TO  JHO-ME03.
**　JAN商品ＣＤ
     MOVE      DEN-F72             TO  JHO-ME04.
**　数量　
     COMPUTE   WK-NOUSU  =   DEN-F15.
     MOVE      WK-NOUSU            TO  JHO-ME08.
**　原価単価　
     COMPUTE   WK-GENTAN  =  DEN-F172.
     MOVE      WK-GENTAN           TO  JHO-ME10.
**　売価単価　
     MOVE      DEN-F173            TO  JHO-ME12.
**　原価金額　
     COMPUTE   WK-GENKIN  =   DEN-F181.
     MOVE      WK-GENKIN           TO  JHO-ME13.
**　売価金額　
     MOVE      DEN-F182            TO  JHO-ME14.
**　商品名
     MOVE      SPACE               TO  JHO-ME16.
**　発注商品ＣＤ
     MOVE      DEN-F25             TO  JHO-ME17.
*-------------------------------
*  明細ラインコード
*-------------------------------
**　レコード区分
     MOVE      "M"                 TO  JHO-MF01.
**　データ種別
     MOVE      DEN-F051            TO  JHO-MF02.
**　規格名
     MOVE      SPACE               TO  JHO-MF03.
**　部門ＣＤ
     MOVE      ZERO                TO  JHO-MF06.
**　商品略名
     MOVE      SPACE               TO  JHO-MF08
**　規格略名
     MOVE      SPACE               TO  JHO-MF09.
**　売価単価
     MOVE      DEN-F173            TO  JHO-MF10.
*　データ出力
     WRITE     JHO-REC.
     ADD       1                   TO  JHO-CNT.
*
 NFJOHOF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ｳﾘｱｹﾞF       READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾃｶﾞｷ ｼｭｯｶｼｼﾞ WT   CNT = " JHO-CNT   UPON CONS.
*
     CLOSE     SHTDENWK  NFJOHOF  SAKUBAF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
