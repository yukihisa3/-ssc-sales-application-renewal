****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム                  *
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　在庫確認書／確定書印刷            *
*    作成日／更新日　　　：　2015/05/25                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　                                  *
*    再利用ＰＧ　　　　　：  SSY3821L.TOKSLIB                  *
****************************************************************
****************************************************************
 IDENTIFICATION          DIVISION.
****************************************************************
 PROGRAM-ID.             SSY3822L.
 AUTHOR.                 NAV.
 DATE-WRITTEN.           2015/05/25.
****************************************************************
 ENVIRONMENT             DIVISION.
****************************************************************
 CONFIGURATION           SECTION.
 SOURCE-COMPUTER.        FACOM.
 OBJECT-COMPUTER.        FACOM.
 SPECIAL-NAMES.
         CONSOLE         IS             CONS.
*
 INPUT-OUTPUT            SECTION.
 FILE-CONTROL.
*    発注書商品明細ワーク
     SELECT   HCSMXXX1     ASSIGN       TO    DA-01-VI-HCSMXXX1
                           ORGANIZATION       INDEXED
                           ACCESS  MODE       SEQUENTIAL
                           RECORD     KEY     SME-F01
                                              SME-F05
                                              SME-F07
                                              SME-F09
                           FILE    STATUS     SME-STATUS.
*    ナフコ店舗マスタ
*    SELECT   NFTENMS1     ASSIGN     TO      DA-01-VI-NFTENMS1
*                          ORGANIZATION       INDEXED
*                          ACCESS     MODE    RANDOM
*                          RECORD     KEY     TEN-F01  TEN-F02
*                          FILE    STATUS     TEN-STATUS.
*    ナフコ商品マスタ
     SELECT   NFSHOMS1     ASSIGN     TO      DA-01-VI-NFSHOMS1
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    RANDOM
                           RECORD     KEY     SHO-F01
                           FILE    STATUS     SHO-STATUS.
*    担当者マスタ
     SELECT   TANMS1       ASSIGN     TO      DA-01-VI-TANMS1
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    RANDOM
                           RECORD     KEY     TAN-F01
                                              TAN-F02
                           FILE    STATUS     TAN-STATUS.
*    プリントファイル
     SELECT  PRTF          ASSIGN    TO       GS-PRTF
                           DESTINATION        "PRT"
                           FORMAT             PRT-FORM
                           GROUP              PRT-GRP
                           PROCESSING         PRT-PROC
                           UNIT CONTROL       PRT-CTL
                           FILE STATUS        PRT-STATUS.
*    作場マスタ
     SELECT   SAKUBAL1     ASSIGN     TO      DA-01-VI-SAKUBAL1
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    RANDOM
                           RECORD     KEY     SAK-F01
                           FILE    STATUS     SAK-STATUS.
*=============================================================*
 DATA                    DIVISION.
*=============================================================*
 FILE                    SECTION.
*    発注書商品明細ワーク
 FD  HCSMXXX1
     LABEL       RECORD    IS        STANDARD.
     COPY        HCSMXXX1  OF        XFDLIB
     JOINING     SME       AS        PREFIX.
*    ナフコ店舗マスタ
*FD  NFTENMS1
*    LABEL       RECORD    IS        STANDARD.
*    COPY        NFTENMS1  OF        XFDLIB
*    JOINING     TEN       AS        PREFIX.
*    ナフコ商品マスタ
 FD  NFSHOMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        NFSHOMS1  OF        XFDLIB
     JOINING     SHO       AS        PREFIX.
*    担当者マスタ
 FD  TANMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        TANMS1    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*    帳票ファイル
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY38221  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
*    作場マスタ
 FD  SAKUBAL1
     LABEL       RECORD    IS        STANDARD.
     COPY        SAKUBAL1  OF        XFDLIB
     JOINING     SAK       AS        PREFIX.
*
*=============================================================*
 WORKING-STORAGE          SECTION.
*=============================================================*
*    制御領域
 01  STATUS-AREA.
     03  SME-STATUS               PIC  X(02).
*    03  TEN-STATUS               PIC  X(02).
     03  SHO-STATUS               PIC  X(02).
     03  TAN-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
     03  SAK-STATUS               PIC  X(02).
*    ＦＯＲＭ制御領域
 01  PRT-FORM                     PIC  X(08).
 01  PRT-PROC                     PIC  X(02).
 01  PRT-GRP                      PIC  X(08).
 01  PRT-CTL.
     03  PRT-CNTRL                PIC  X(04).
     03  PRT-STR-PG               PIC  X(02).
*    フラグエリア
 01  FLG-AREA.
     03  FLG-END                  PIC  X(03)  VALUE  SPACE.
     03  FLG-READ                 PIC  X(03)  VALUE  SPACE.
*    03  FLG-TK                   PIC  X(02)  VALUE  SPACE.
*    退避エリア
 01  SAV-AREA.
     03  WK-SME-F01               PIC  9(08)   VALUE  ZERO.
     03  WK-SME-F05               PIC  X(13)   VALUE  ZERO.
     03  WK-SME-F12               PIC  X(02)   VALUE  SPACE.
     03  WK-SME-F06               PIC  9(08)   VALUE  ZERO.
     03  WK-SME-F07               PIC  9(08)   VALUE  ZERO.
     03  PAGE-CNT                 PIC  9(04)   VALUE  ZERO.
     03  TATE                     PIC  9(02)   VALUE  ZERO.
     03  YOKO                     PIC  9(02)   VALUE  ZERO.
     03  IX                       PIC  9(02)   VALUE  ZERO.
     03  IY                       PIC  9(02)   VALUE  ZERO.
     03  IZ                       PIC  9(01)   VALUE  ZERO.
     03  WK-HENKAN                PIC  N(01)   VALUE  SPACE.
     03  CHK-FLG                  PIC  9(01)   VALUE  ZERO.
     03  WK-GK-TRAY               PIC  9(06)V9 VALUE  ZERO.
     03  WK-GK-SURYO              PIC  9(06)   VALUE  ZERO.
     03  WK-DATE                  PIC  9(01)   VALUE  ZERO.
     03  READ-CNT                 PIC  9(06)   VALUE  ZERO.
     03  BK-HACU     OCCURS 20    PIC  9(08)   VALUE  ZERO.
     03  BK-TRAY     OCCURS 20    PIC  9(08)   VALUE  ZERO.
*    集計エリア
 01  SUM-AREA.
     03  SUM-GGOKEI               PIC  9(08)   VALUE  ZERO.
     03  SUM-GTRESU               PIC  9(08)V9 VALUE  ZERO.
     03  SUM-KEIHAK               PIC  9(08)   VALUE  ZERO.
*    ルート毎店舗情報ワークエリア
*01  WK-TENPO.
*    03  TENPO                    OCCURS      12.
*        05  TENCD                PIC  9(04)  VALUE  ZERO.
*        05  TENMEI               PIC  N(03)  VALUE  SPACE.
*    ルート番号退避
 01  WK-KAKUNOU.
     03  WK-SME-F07NO             PIC  9(02)  VALUE  ZERO.
*    エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*    漢字変換
 01  WK-KANJI.
     03  KANJI                    PIC  N(10)  VALUE
         NC"１２３４５６７８９０".
 01  WK-KANJIR                    REDEFINES   WK-KANJI.
     03  WK-SU                    OCCURS      10.
         05  SU                   PIC  N(01).
*    システム日付
 01  WRK-DATE.
     03  WRK-DATE1                PIC  9(02)  VALUE  ZERO.
     03  WRK-DATE2                PIC  9(06)  VALUE  ZERO.
*     システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
*    漢字ワークエリア
 01  WK-HENKAN-KANJI.
     03  WK-HENK1.
         05  WK-HENK01            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK02            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK11            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK12            PIC  N(01)  VALUE  SPACE.
     03  WK-FIL1                  PIC  N(01)  VALUE  NC"／".
     03  WK-HENK2.
         05  WK-HENK21            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK22            PIC  N(01)  VALUE  SPACE.
     03  WK-FIL2                  PIC  N(01)  VALUE  NC"／".
     03  WK-HENK3.
         05  WK-HENK31            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK32            PIC  N(01)  VALUE  SPACE.
*    合計金額格納エリア
 01  GOKEI-AREA.
     03  GOKEI-GENKA              OCCURS      13     TIMES.
         05  GENKEI               PIC S9(09).
     03  GOKEI-BAIKA              OCCURS      13     TIMES.
         05  BAIKEI               PIC S9(09).
*    商品名編集エリア
 01  SYOHSME-AREA.
     03  SYOHSME-1.
         05  WK-SYOHSME-11        PIC  X(14).
         05  WK-SYOHSME-12        PIC  X(01).
     03  SYOHSME-2.
         05  WK-SYOHSME-21        PIC  X(01).
         05  WK-SYOHSME-22        PIC  X(14).
*    部門・担当者編集エリア
 01  BUMTANSME-AREA.
     03  WK-BUMON                 PIC  N(04).
     03  WK-BUTANFIL1             PIC  N(01)  VALUE NC"－".
     03  WK-TANTOU                PIC  N(02).
     03  WK-BUTANFIL2             PIC  N(01)  VALUE NC"　".
     03  WK-TANTOUNM              PIC  N(12).
*    作場ＣＤ編集エリア
 01  SAKUBASME-AREA.
     03  WK-SAKUBA                PIC  N(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-PG-ID            PIC  X(08)  VALUE  "SSY3822L".
         05  FILLER               PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID            PIC  X(08).
         05  FILLER               PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD             PIC  X(02).
         05  FILLER               PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-SME-KBN           PIC X(01).
 01  LINK-SME-YMD6          PIC 9(06).
 01  LINK-SME-YMD8          PIC 9(08).
 01  LINK-OUT-RET           PIC X(01).
 01  LINK-OUT-YMD           PIC 9(08).
*
 LINKAGE                 SECTION.
 01  LINK-SYUPTN            PIC  X(01).
 01  LINK-BUMON             PIC  X(04).
 01  LINK-TANTOU            PIC  X(02).
*============================================================*
 PROCEDURE               DIVISION
                         USING    LINK-SYUPTN
                                  LINK-BUMON
                                  LINK-TANTOU.
*============================================================*
 DECLARATIVES.
*    プリントファイル
 FILEERR-SEC1           SECTION.
     USE AFTER EXCEPTION PROCEDURE   PRTF.
     MOVE      "PRTF"           TO   ERR-FL-ID.
     MOVE      PRT-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　発注書商品明細ワーク
 FILEERR-SEC2           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HCSMXXX1.
     MOVE      "HCSMXXX1"         TO   ERR-FL-ID.
     MOVE      SME-STATUS        TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　ナフコ店舗マスタ
*FILEERR-SEC3           SECTION.
*    USE AFTER EXCEPTION PROCEDURE   NFTENMS1.
*    MOVE      "NFTENMS1"         TO   ERR-FL-ID.
*    MOVE      TEN-STATUS       TO   ERR-STCD.
*    DISPLAY   MSG-ABEND1       UPON CONS.
*    DISPLAY   MSG-ABEND2       UPON CONS.
*    DISPLAY   SEC-NAME         UPON CONS.
*    MOVE     "4000"            TO   PROGRAM-STATUS.
*    STOP     RUN.
*　　ナフコ商品マスタ
 FILEERR-SEC4           SECTION.
     USE AFTER EXCEPTION PROCEDURE   NFSHOMS1.
     MOVE      "NFSHOMS1"        TO   ERR-FL-ID.
     MOVE      SHO-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　担当者マスタ
 FILEERR-SEC4           SECTION.
     USE AFTER EXCEPTION PROCEDURE   TANMS1.
     MOVE      "TANMS1"         TO   ERR-FL-ID.
     MOVE      TAN-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　作場マスタ
 FILEERR-SEC6           SECTION.
     USE AFTER EXCEPTION PROCEDURE   SAKUBAL1.
     MOVE      "SAKUBAL1"        TO   ERR-FL-ID.
     MOVE      SAK-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
*
*============================================================*
*　　ゼネラル処理　　　　　　　　　　　　  構造_0.0         *
*============================================================*
 CONTROL-SEC             SECTION.
     MOVE     "COTROL-SEC"        TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL    FLG-END  =  "END".
     PERFORM  END-SEC.
     STOP     RUN.
*
 CONTROL-EXIT.
     EXIT.
*============================================================*
*　　初期処理　　　　　　　　　　　　　　  構造_1.0         *
*============================================================*
 INIT-SEC                SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*    使用ファイル　ＯＰＥＮ
     OPEN     INPUT      HCSMXXX1 NFSHOMS1 TANMS1
                         SAKUBAL1.
     OPEN     OUTPUT     PRTF.
*    システム日付の取得
*システム日付・時刻の取得
     ACCEPT   WRK-DATE2         FROM   DATE.
     MOVE     "3"                 TO   LINK-SME-KBN.
     MOVE     WRK-DATE2           TO   LINK-SME-YMD6.
     MOVE     ZERO                TO   LINK-SME-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-SME-KBN
                                       LINK-SME-YMD6
                                       LINK-SME-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WRK-DATE.
*
     ACCEPT   SYS-TIME  FROM      TIME.
*    プリントエリア初期化
     MOVE     SPACE      TO       PRT-FSY38221.
     MOVE     ZERO       TO       READ-CNT.
*    MOVE     ZERO       TO       GOKEI-AREA.
*    MOVE     SPACE      TO       FLG-TK.
*    発注書商品明細ワーク初期ＲＥＡＤ
     PERFORM  HCSMXXX1-READ-SEC.
     MOVE     SME-F01     TO       WK-SME-F01.
     MOVE     SME-F05     TO       WK-SME-F05.
     MOVE     SME-F06     TO       WK-SME-F06.
     MOVE     SME-F02     TO       WK-SME-F06.
     MOVE     SME-F07     TO       WK-SME-F07.
     MOVE     0           TO       TATE.
*    PERFORM  TEN-SET-SEC.
*
 INIT-EXT.
     EXIT.
*============================================================*
*　　発注書商品明細ワーク読込み　　　　　  構造_            *
*============================================================*
 HCSMXXX1-READ-SEC       SECTION.
     MOVE     "HCSMXXX1-READ-SEC"   TO   S-NAME.
*    発注書商品明細ワーク読込み
     READ     HCSMXXX1   AT  END
*             IF READ-CNT > ZERO
*          ?     MOVE      WK-GK-SURYO     TO     PRT-SOGK(TATE)
*          ?     MOVE      "ON"            TO     FLG-TK
*          ?     PERFORM   PRINT-SEC   最終ページ
*             END-IF
              MOVE     "END"      TO       FLG-END
              GO                  TO       HCSMXXX1-READ-EXIT
     END-READ.
 READ010.
*    対象データ件数カウント
     ADD      1                   TO       READ-CNT.
*
 HCSMXXX1-READ-EXIT.
     EXIT.
*============================================================*
*　　メイン処理　　　　　　　　　　　　　  構造_2.0         *
*============================================================*
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-01.
*    ブレイクチェック（管理番号・作場・店着日）
     IF       WK-SME-F01  NOT =  SME-F01
         OR   WK-SME-F05  NOT =  SME-F05
         OR   WK-SME-F07  NOT =  SME-F07
              MOVE  WK-GK-TRAY   TO  PRT-GKTRIS
*             COMPUTE PRT-GKTRIS =   WK-GK-TRAY + 0.9
              MOVE  WK-GK-SURYO  TO  PRT-GKGOKI
*         ?   MOVE  "ON"         TO  FLG-TK
              PERFORM            PRINT-SEC
              MOVE  SME-F01      TO  WK-SME-F01
              MOVE  SME-F05      TO  WK-SME-F05
              MOVE  SME-F06      TO  WK-SME-F06
              MOVE  SME-F02      TO  WK-SME-F06
              MOVE  SME-F07      TO  WK-SME-F07
              MOVE  1            TO  TATE
              MOVE  ZERO         TO  WK-GK-TRAY
              MOVE  ZERO         TO  WK-GK-SURYO
*             PERFORM            TEN-SET-SEC
              GO    TO           MAIN-02
     END-IF.
*    発注数合計レコード時
*    IF      SME-F13  =  99997
*       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12
*            MOVE      SME-F202(IX) TO  PRT-GGOKEI(IX)
*            ADD       SME-F202(IX) TO  SUM-GGOKEI
*       END-PERFORM
*       MOVE SUM-GGOKEI              TO    PRT-GGOKEI(13)
*       MOVE ZERO                    TO    SUM-GGOKEI
*       GO   TO       MAIN-04
*    END-IF.
*    トレー数合計レコード時
*    IF      SME-F13  =  99998
*       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12
*            MOVE      SME-F202(IX)    TO  PRT-GTRESU(IX)
*            COMPUTE   PRT-GTRESU(IX)  =   SME-F202(IX) + 0.9
*            ADD       PRT-GTRESU(IX)  TO  SUM-GTRESU
*       END-PERFORM
*       MOVE SUM-GTRESU              TO    PRT-GTRESU(13)
*       MOVE ZERO                    TO    SUM-GTRESU
*       GO   TO       MAIN-04
*    END-IF.
*    箱数合計レコード時
*    IF      SME-F13  =  99999
*       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12
*            MOVE      SME-F202(IX)  TO  PRT-KEIHAK(IX)
*            ADD       SME-F202(IX)  TO  SUM-KEIHAK
*       END-PERFORM
*       MOVE SUM-KEIHAK              TO  PRT-KEIHAK(13)
*       MOVE ZERO                    TO  SUM-KEIHAK
*       MOVE 20                      TO  TATE
*       GO   TO       MAIN-04
*    END-IF.
*
     ADD      1                      TO  TATE.
*TEST
*    DISPLAY "TATE=" TATE UPON CONS.
*TEST
*
 MAIN-02.
*    同頁内で商品が4０アイテム以上の時，次頁へ改頁
     IF       TATE  >  20
              PERFORM            PRINT-SEC
*             MOVE  SME-F01      TO  WK-SME-F01
*             MOVE  SME-F05      TO  WK-SME-F05
*             MOVE  SME-F06      TO  WK-SME-F06
*             MOVE  SME-F07      TO  WK-SME-F07
*             MOVE  SME-F12      TO  WK-SME-F12
              MOVE  1            TO  TATE
*             MOVE  ZERO         TO  WK-GK-TRAY
*             MOVE  ZERO         TO  WK-GK-SURYO
*             PERFORM            TEN-SET-SEC
      END-IF.
 MAIN-03.
*    明細項目セット処理へ
     PERFORM  DATA-SET-SEC.
 MAIN-04.
*    発注書商品明細ワーク読込み
     PERFORM  HCSMXXX1-READ-SEC.
     IF       FLG-END  =  "END"
              MOVE  WK-GK-TRAY   TO  PRT-GKTRIS
*             COMPUTE PRT-GKTRIS =   WK-GK-TRAY + 0.9
              MOVE  WK-GK-SURYO  TO  PRT-GKGOKI
              PERFORM            PRINT-SEC
     END-IF.
*
 MAIN-EXT.
     EXIT.
*============================================================*
*　　終了処理　　　　　　　　　　　　　　  構造_3.0         *
*============================================================*
 END-SEC                 SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*    使用ファイルＣＬＯＳＥ
     CLOSE                        HCSMXXX1 TANMS1 NFSHOMS1
                                  SAKUBAL1
                                  PRTF.
*    終了メッセージ
     IF LINK-SYUPTN = "1"
     DISPLAY "****************************" UPON CONS
     DISPLAY "*    在庫確認書印刷　　    *" UPON CONS
     DISPLAY "*  ｼｭﾂﾘｮｸ ﾏｲｽｳ = " PAGE-CNT "      *" UPON CONS
     DISPLAY "****************************" UPON CONS
     END-IF.
     IF LINK-SYUPTN = "2"
     DISPLAY "****************************" UPON CONS
     DISPLAY "*    在庫確定書印刷　　    *" UPON CONS
     DISPLAY "*  ｼｭﾂﾘｮｸ ﾏｲｽｳ = " PAGE-CNT "      *" UPON CONS
     DISPLAY "****************************" UPON CONS
     END-IF.
*
 END-EXT.
     EXIT.
*============================================================*
*　　店舗ＣＤ・名称セット　                構造_1.1         *
*============================================================*
*TEN-SET-SEC          SECTION.
*    MOVE     "TEN-SET-SEC"   TO     S-NAME.
*
*    MOVE     ZERO            TO     IX.
*    INITIALIZE               WK-TENPO.
*    MOVE  SPACE              TO     WK-TENPO.
*
*TEN010.
*    PERFORM VARYING IX FROM   1  BY   1
*            UNTIL ( IX > 12 )    OR ( SME-F201(IX) = 0 )
*       MOVE    SME-F04       TO     TEN-F01
*       MOVE    SME-F201(IX)  TO     TEN-F02
*       READ    NFTENMS1
*               INVALID
*                  MOVE   TEN-F02    TO  PRT-MTENCD(IX)
*                  MOVE   ALL NC"？" TO  PRT-MTENNM(IX)
*               NOT INVALID
*                  MOVE   TEN-F02    TO  PRT-MTENCD(IX)
*                  MOVE   TEN-F05    TO  PRT-MTENNM(IX)
*       END-READ
*    END-PERFORM.
*
*TEN020.
* 商品最終ページ判定→"次頁" OR "合計"
*    IF SME-F23 = "1"
*       MOVE  NC"合計"        TO     PRT-MGOTIL
*    ELSE
*       MOVE  NC"次頁"        TO     PRT-MGOTIL
*    END-IF.
*↓試し
*    IF SME-F12 = 2
*       MOVE  NC"合計"        TO     PRT-MGOTIL
*    ELSE
*       MOVE  NC"次頁"        TO     PRT-MGOTIL
*    END-IF.
*↑試し
*
*TEN-SET-EXIT.
*    EXIT.
*============================================================*
*    帳票出力処理　　　　　　　　　　      構造_2.1         *
*============================================================*
 PRINT-SEC         SECTION.
*
     MOVE     "PRINT-SEC"     TO   S-NAME.
*
*    帳票名
     EVALUATE LINK-SYUPTN
        WHEN  "1"    MOVE  NC"在庫確認書" TO  PRT-ZAITIL
        WHEN  "2"    MOVE  NC"在庫確定書" TO  PRT-ZAITIL
        WHEN  OTHER  MOVE  NC"？？？？？" TO  PRT-ZAITIL
     END-EVALUATE.
*    ページ　カウントアップ
     ADD      1               TO   PAGE-CNT.
     MOVE     PAGE-CNT        TO   PRT-HPAGE.
*    システム日付セット
     MOVE     WRK-DATE(1:4)   TO   PRT-HDATE(1:4).
     MOVE     "/"             TO   PRT-HDATE(5:1).
     MOVE     WRK-DATE(5:2)   TO   PRT-HDATE(6:2).
     MOVE     "/"             TO   PRT-HDATE(8:1).
     MOVE     WRK-DATE(7:2)   TO   PRT-HDATE(9:2).
*    システム時刻セット
     MOVE     SYS-HH          TO   PRT-HTIME(1:2).
     MOVE     ":"             TO   PRT-HTIME(3:1).
     MOVE     SYS-MM          TO   PRT-HTIME(4:2).
*    作場名セット
     MOVE   WK-SME-F05        TO   SAK-F01.
     READ   SAKUBAL1
        INVALID
            MOVE      ALL NC"＊" TO   PRT-HSAKNM
        NOT INVALID
            MOVE      SAK-F02    TO   PRT-HSAKNM
     END-READ.
*
*    作場ＣＤ漢字変換→セット
*    PERFORM  VARYING IZ FROM 1 BY 1 UNTIL IZ > 2
*             PERFORM SAKUBA-HENKAN-SEC
*    END-PERFORM.
*    MOVE     SAKUBASME-AREA     TO   PRT-HSAKCD.
     MOVE     "("                TO   PRT-HSAKCD(1:1).
*****MOVE     SME-F05            TO   PRT-HSAKCD(2:2).
     MOVE     WK-SME-F05(1:2)    TO   PRT-HSAKCD(2:2).
     MOVE     ")"                TO   PRT-HSAKCD(4:1).
*
*    出荷日漢字変換→セット
     PERFORM  VARYING IZ FROM 1 BY 1 UNTIL IZ > 8
              PERFORM KANJI-HENKAN-SEC
     END-PERFORM.
*    変換文字転送
     MOVE   WK-HENKAN-KANJI      TO   PRT-HSYKDT.
*
*    部門ＣＤ漢字変換
     PERFORM  VARYING IZ FROM 1 BY 1 UNTIL IZ > 4
              PERFORM BUMON-HENKAN-SEC
     END-PERFORM.
*    担当者ＣＤ漢字変換
     PERFORM  VARYING IZ FROM 1 BY 1 UNTIL IZ > 2
              PERFORM TANTOU-HENKAN-SEC
     END-PERFORM.
*    担当者名セット
     MOVE   LINK-BUMON  TO       TAN-F01.
     MOVE   LINK-TANTOU TO       TAN-F02.
     READ   TANMS1
        INVALID
            MOVE      ALL NC"＊" TO   WK-TANTOUNM
        NOT INVALID
            MOVE      TAN-F03    TO   WK-TANTOUNM
     END-READ.
*
     MOVE     BUMTANSME-AREA      TO   PRT-IRAISY.
*
*    バッチナンバー セット
     MOVE     SME-F02             TO   PRT-BTDATE.
     MOVE     SME-F03             TO   PRT-BTTIME.
     MOVE     SME-F04             TO   PRT-BTTCD.
*    管理番号 セット
     MOVE     SME-F01             TO   PRT-BTKANR.
*
*    印字制御項目セット
     MOVE     SPACE               TO   PRT-PROC.
     MOVE     SPACE               TO   PRT-CTL.
     MOVE     SPACE               TO   PRT-FORM.
     MOVE    "FSY38221"           TO   PRT-FORM.
     MOVE    "SCREEN"             TO   PRT-GRP.
*    帳票出力
 SYUKEI010.
     WRITE    PRT-FSY38221.
*    プリントエリア初期化
     MOVE     SPACE               TO   PRT-FSY38221.
*
 PRINT-EXIT.
     EXIT.
*============================================================*
*　　作場変換処理　　　　　                構造_2.1.1       *
*============================================================*
*SAKUBA-HENKAN-SEC      SECTION.
*    MOVE     "SAKUBA-HENKAN-SEC"  TO   S-NAME.
*    英数字を漢字に変換
*    EVALUATE  SME-F05(IZ:1)
*      WHEN   "A"
*              MOVE    NC"Ａ"          TO     PRT-HSAKCD(IZ:1)
*              MOVE    NC"Ａ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "B"
*              MOVE    NC"Ｂ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "C"
*              MOVE    NC"Ｃ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "D"
*              MOVE    NC"Ｄ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "E"
*              MOVE    NC"Ｅ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "F"
*              MOVE    NC"Ｆ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "G"
*              MOVE    NC"Ｇ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "H"
*              MOVE    NC"Ｈ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "I"
*              MOVE    NC"Ｉ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "J"
*              MOVE    NC"Ｊ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "K"
*              MOVE    NC"Ｋ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "L"
*              MOVE    NC"Ｌ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "M"
*              MOVE    NC"Ｍ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "N"
*              MOVE    NC"Ｎ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "O"
*              MOVE    NC"Ｏ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "P"
*              MOVE    NC"Ｐ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "Q"
*              MOVE    NC"Ｑ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "R"
*              MOVE    NC"Ｒ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "S"
*              MOVE    NC"Ｓ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "T"
*              MOVE    NC"Ｔ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "U"
*              MOVE    NC"Ｕ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "V"
*              MOVE    NC"Ｖ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "W"
*              MOVE    NC"Ｗ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "X"
*              MOVE    NC"Ｘ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "Y"
*              MOVE    NC"Ｙ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "Z"
*              MOVE    NC"Ｚ"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "0"
*              MOVE    NC"０"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "1"
*              MOVE    NC"１"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "2"
*              MOVE    NC"２"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "3"
*              MOVE    NC"３"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "4"
*              MOVE    NC"４"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "5"
*              MOVE    NC"５"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "6"
*              MOVE    NC"６"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "7"
*              MOVE    NC"７"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "8"
*              MOVE    NC"８"          TO     WK-SAKUBA(IZ:1)
*      WHEN   "9"
*              MOVE    NC"９"          TO     WK-SAKUBA(IZ:1)
*    END-EVALUATE.
*
*SAKUBA-HENKAN-EXIT.
*    EXIT.
*============================================================*
*　　担当者コード変換　　　　　　          構造_2.1.1       *
*============================================================*
 TANTOU-HENKAN-SEC      SECTION.
     MOVE     "TANTOU-HENKAN-SEC"  TO   S-NAME.
*    英数字を漢字に変換
     EVALUATE  LINK-TANTOU(IZ:1)
       WHEN   "A"
               MOVE    NC"Ａ"          TO     WK-TANTOU(IZ:1)
       WHEN   "B"
               MOVE    NC"Ｂ"          TO     WK-TANTOU(IZ:1)
       WHEN   "C"
               MOVE    NC"Ｃ"          TO     WK-TANTOU(IZ:1)
       WHEN   "D"
               MOVE    NC"Ｄ"          TO     WK-TANTOU(IZ:1)
       WHEN   "E"
               MOVE    NC"Ｅ"          TO     WK-TANTOU(IZ:1)
       WHEN   "F"
               MOVE    NC"Ｆ"          TO     WK-TANTOU(IZ:1)
       WHEN   "G"
               MOVE    NC"Ｇ"          TO     WK-TANTOU(IZ:1)
       WHEN   "H"
               MOVE    NC"Ｈ"          TO     WK-TANTOU(IZ:1)
       WHEN   "I"
               MOVE    NC"Ｉ"          TO     WK-TANTOU(IZ:1)
       WHEN   "J"
               MOVE    NC"Ｊ"          TO     WK-TANTOU(IZ:1)
       WHEN   "K"
               MOVE    NC"Ｋ"          TO     WK-TANTOU(IZ:1)
       WHEN   "L"
               MOVE    NC"Ｌ"          TO     WK-TANTOU(IZ:1)
       WHEN   "M"
               MOVE    NC"Ｍ"          TO     WK-TANTOU(IZ:1)
       WHEN   "N"
               MOVE    NC"Ｎ"          TO     WK-TANTOU(IZ:1)
       WHEN   "O"
               MOVE    NC"Ｏ"          TO     WK-TANTOU(IZ:1)
       WHEN   "P"
               MOVE    NC"Ｐ"          TO     WK-TANTOU(IZ:1)
       WHEN   "Q"
               MOVE    NC"Ｑ"          TO     WK-TANTOU(IZ:1)
       WHEN   "R"
               MOVE    NC"Ｒ"          TO     WK-TANTOU(IZ:1)
       WHEN   "S"
               MOVE    NC"Ｓ"          TO     WK-TANTOU(IZ:1)
       WHEN   "T"
               MOVE    NC"Ｔ"          TO     WK-TANTOU(IZ:1)
       WHEN   "U"
               MOVE    NC"Ｕ"          TO     WK-TANTOU(IZ:1)
       WHEN   "V"
               MOVE    NC"Ｖ"          TO     WK-TANTOU(IZ:1)
       WHEN   "W"
               MOVE    NC"Ｗ"          TO     WK-TANTOU(IZ:1)
       WHEN   "X"
               MOVE    NC"Ｘ"          TO     WK-TANTOU(IZ:1)
       WHEN   "Y"
               MOVE    NC"Ｙ"          TO     WK-TANTOU(IZ:1)
       WHEN   "Z"
               MOVE    NC"Ｚ"          TO     WK-TANTOU(IZ:1)
       WHEN   "0"
               MOVE    NC"０"          TO     WK-TANTOU(IZ:1)
       WHEN   "1"
               MOVE    NC"１"          TO     WK-TANTOU(IZ:1)
       WHEN   "2"
               MOVE    NC"２"          TO     WK-TANTOU(IZ:1)
       WHEN   "3"
               MOVE    NC"３"          TO     WK-TANTOU(IZ:1)
       WHEN   "4"
               MOVE    NC"４"          TO     WK-TANTOU(IZ:1)
       WHEN   "5"
               MOVE    NC"５"          TO     WK-TANTOU(IZ:1)
       WHEN   "6"
               MOVE    NC"６"          TO     WK-TANTOU(IZ:1)
       WHEN   "7"
               MOVE    NC"７"          TO     WK-TANTOU(IZ:1)
       WHEN   "8"
               MOVE    NC"８"          TO     WK-TANTOU(IZ:1)
       WHEN   "9"
               MOVE    NC"９"          TO     WK-TANTOU(IZ:1)
     END-EVALUATE.
*
 TANTOU-HENKAN-EXIT.
     EXIT.
*============================================================*
*　　漢字変換処理（出荷日）                構造_2.1.1       *
*============================================================*
 KANJI-HENKAN-SEC      SECTION.
     MOVE     "KANJI-HENKAN-SEC"  TO   S-NAME.
*    数字を漢字に変換（ワーク変換テーブルにより）
     MOVE    WK-SME-F06(IZ:1)          TO     WK-DATE.
     IF   (  WK-DATE  =  ZERO  )
     AND  (  IZ       =  3  OR  5  OR  7  )
             MOVE  SPACE               TO     WK-HENKAN
     ELSE
             MOVE  WK-DATE             TO     IY
             IF    IY  =  ZERO
                   MOVE   10           TO     IY
             END-IF
             MOVE  SU(IY)              TO     WK-HENKAN
     END-IF.
*    変換後転送
     EVALUATE IZ
         WHEN 1
              MOVE WK-HENKAN           TO     WK-HENK01
         WHEN 2
              MOVE WK-HENKAN           TO     WK-HENK02
         WHEN 3
              MOVE WK-HENKAN           TO     WK-HENK11
         WHEN 4
              MOVE WK-HENKAN           TO     WK-HENK12
         WHEN 5
              MOVE WK-HENKAN           TO     WK-HENK21
         WHEN 6
              MOVE WK-HENKAN           TO     WK-HENK22
         WHEN 7
              MOVE WK-HENKAN           TO     WK-HENK31
         WHEN 8
              MOVE WK-HENKAN           TO     WK-HENK32
     END-EVALUATE.
*
 KANJI-HENKAN-EXIT.
     EXIT.
*============================================================*
*　　漢字変換処理（部門ＣＤ）              構造_2.1.1       *
*============================================================*
 BUMON-HENKAN-SEC      SECTION.
     MOVE     "BUMON-HENKAN-SEC"  TO   S-NAME.
*    数字を漢字に変換（ワーク変換テーブルにより）
     MOVE    LINK-BUMON(IZ:1)          TO     IY.
     IF      IY  =  ZERO
             MOVE  10                  TO     IY
     END-IF.
     MOVE    SU(IY)                    TO     WK-HENKAN.
*    変換後転送
     EVALUATE IZ
         WHEN 1
              MOVE WK-HENKAN           TO     WK-BUMON(1:1)
         WHEN 2
              MOVE WK-HENKAN           TO     WK-BUMON(2:1)
         WHEN 3
              MOVE WK-HENKAN           TO     WK-BUMON(3:1)
         WHEN 4
              MOVE WK-HENKAN           TO     WK-BUMON(4:1)
     END-EVALUATE.
*
 BUMON-HENKAN-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 DATA-SET-SEC            SECTION.
     MOVE     "DATA-SET-SEC"      TO   S-NAME.
*TEST
*    DISPLAY "TATE=" TATE UPON CONS.
*TEST
*    サカタコード
     MOVE    SME-F10      TO        PRT-MSKTCD(TATE).
*    相手コード
     MOVE    SME-F09      TO        PRT-MJANCD(TATE).
*    商品名,規格
     MOVE    SME-F09     TO         SHO-F01.
     READ    NFSHOMS1
        INVALID
             MOVE      ALL NC"＊" TO   PRT-MSYONM(TATE)
             MOVE      ALL NC"＊" TO   PRT-MKIKNM(TATE)
        NOT INVALID
             MOVE      SHO-F05    TO   PRT-MSYONM(TATE)
             MOVE      SHO-F06    TO   PRT-MKIKNM(TATE)
     END-READ.
*    入数
     MOVE    SME-F12      TO        PRT-MIRISU(TATE).
*    トレー数商品合計
*    MOVE    SME-F22      TO        PRT-MTORSU(TATE).
     COMPUTE PRT-MTORSU(TATE)  =    SME-F22 + 0.9.
*    発注数商品合計
     MOVE    SME-F21      TO        PRT-MGOKEI(TATE).
*    トレー数合計集計
     ADD     PRT-MTORSU(TATE)       TO        WK-GK-TRAY.
*    発注数合計集計
     ADD     SME-F21      TO        WK-GK-SURYO.
*
*    発注数
*    PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12
*            MOVE      SME-F202(IX) TO  PRT-MSURYO(TATE IX)
*    END-PERFORM.
* 商品最終ページ判定
*    IF SME-F23 = "1"
*       MOVE    SME-F21       TO     PRT-MSURYO(TATE 13)
*       MOVE    SME-F22       TO     PRT-MSURYO(TATE 14)
*       COMPUTE PRT-MSURYO(TATE 14) = SME-F22 + 0.9
*    END-IF.
*
 DATA-SET-EXIT.
     EXIT.
*============================================================*
*　　合計セット（店舗毎）　                構造_2.1.2       *
*============================================================*
*GOKEI-SET-SEC         SECTION.
*    MOVE     "GOKEI-SET-SEC"     TO   S-NAME.
*   テーブルよりプリントエリアへデータセット
*    PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 13
*       IF   GENKEI(IX)  NOT =  ZERO
*            COMPUTE PRT-GENKEI(IX) = GENKEI(IX) / 1000
*       END-IF
*       IF   BAIKEI(IX)  NOT =  ZERO
*            COMPUTE PRT-BAIKEI(IX) = BAIKEI(IX) / 1000
*       END-IF
*       IF   GENKEI(IX)  NOT =  ZERO
*            COMPUTE PRT-NEIRE (IX) ROUNDED =
*               ( 1 - ( GENKEI(IX) / BAIKEI(IX) ))  * 100
*       END-IF
*    END-PERFORM.
*
*GOKEI-SET-EXIT.
*    EXIT.
